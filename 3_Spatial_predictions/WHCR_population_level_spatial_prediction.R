library(foreach)
library(doParallel)
library(raster)
library(rgdal)
library(sp)
library(lme4)
library(AICcmodavg)
library(boot)
library(arm)
library(MASS)
library(glmnet)
library(ggplot2)
library(GGally)
library(ggthemes)
library(pscl)
library(gbm)
library("rpart")
library("caretEnsemble")
library(pROC)
library(randomForest)
library(caret)

######################################################
##################### load models#####################
######################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("pop_level_meta_model_5_30_23.RDS")
preProcValues <- readRDS("preProcValues_population_5_30_23.RDS")
testdata <- readRDS("testTransformed_population_5_30_23.RDS")
traindata <- readRDS("trainTransformed_population_5_30_23.RDS" )
data1_trn <- readRDS("untrans_population_5_30_23.RDS")
data.smote <- readRDS("smote_population_5_30_23.RDS")

########################################################################
##### Spatial prediction ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")

rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
index <- which(names(all)=="d_data4")
names(all)[index] <- "d_data"
index <- which(names(all)=="sorghum__1000")
names(all)[index] <- "sorghum_1000"

index <- which(names(all) %in% names(data.smote))

all <- all[[index]]


model.predict <- function(newdat){
  1- predict(meta_model, newdat, type = "prob")
}

startTime=date()
startTime

xseq <- seq(1695847, 1909157, length.out = 25)

for(i in 1:24){
  print(i)
  
  alli <- crop(all, extent(xseq[i], xseq[i+1], 7043218, 7240138))
  print("crop done")
  p <- data.frame(rasterToPoints(alli))
  print("raster to points done")
  #p <- na.omit(p)
  names(p)
  if(dim(p)[1]==0)next
  
  chunk1 <- seq(1, length(p[,1]), by=1000)
  result.1 <-
    foreach(a=chunk1)%dopar% {
      samp.data <- p[a:(a+999),]
      samp.data <- as.data.frame(samp.data)
      samp.data <- predict(preProcValues, samp.data)
      ## NAs cause problems in predictions
      #index of NA rows
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros
      samp.data[is.na(samp.data)] <- 0
      # predict
      data_out <- 1- predict(meta_model, samp.data, type = "prob")
      # replace predictions with NA, if row contained NA
      data_out[index] <- NA
      cat('\r',a/dim(p)[1]*100); flush.console() 
      return(data_out)
    } 
  stopTime=date()
  
  gcw.mod <- unlist(result.1, use.names = FALSE) # Make list into data frame
  gcw.mod <- gcw.mod[1:length(p[,1])]
  gcw.mod1 <- cbind(p[1:length(p[,1]),1:2], gcw.mod) # Combine results with xy coordinates
  
  colnames(gcw.mod1) <- c("x", "y", "dens")
  
  gcw.mod2 <- rasterFromXYZ(gcw.mod1[,c("x", "y", "dens")]) # Make raster of predicted density
  
  setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/pop_level_products")
  writeRaster(gcw.mod2, filename=paste("whcr_pop_6_9_23", i, "_.tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/pop_level_products")

list2 <- list()
for(i in 1:24){ 
  rx <- raster(paste("whcr_pop_6_9_23", i, "_.tif", sep=""))
  list2[[i]] <- rx
}

list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/pop_level_products")
writeRaster(rast.mosaic, filename = "population_level_prediction_6_9_23.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)