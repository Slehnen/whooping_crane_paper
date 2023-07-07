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
##################### Random data generation #########
######################################################


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("HR_level_meta_model_5_18_2023.RDS")
preProcValues <- readRDS("preProcValues_HR_5_18_23.RDS")
data1_trn <- readRDS("data1_trn_HR_5_18_23.RDS")
testTransformed <- readRDS("testTransformed_HR_5_18_23.RDS")
trainTransformed <- readRDS("trainTransformed_HR_5_18_23.RDS")


########################################################################
##### Spatial prediction ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)

index <- which(names(all)=="water_500")
names(all)[index] <- "water_500.2"
index <- which(names(all)=="TWI")
names(all)[index] <- "twi"
index <- which(names(all)=="sosn_500")
names(all)[index] <- "SOSN_500"
index <- which(names(all)=="barth_dem_125_resample")
names(all)[index] <- "barth_dem_125"

index <- which(names(all) %in% names(trainTransformed))

all <- all[[index]]


xseq <- seq(1695847, 1909157, length.out = 25)

for(i in 2:24){
  print(i)
  alli <- crop(all, extent(xseq[i], xseq[i+1], 7043218, 7240138))
  print("crop done")
  p <- data.frame(rasterToPoints(alli))
  print("raster to points done")
  if(dim(p)[1]==0)next
  
  library(foreach)
  
  chunk1 <- seq(1, length(p[,1]), by=1000)
  startTime=date()
  result.1 <-
    foreach(a=chunk1)%dopar% {
      samp.data <- p[a:(a+999),]
      samp.data <- as.data.frame(samp.data)
      samp.data <- samp.data[,3:dim(samp.data)[2]]
      samp.data$d_data4 <- median(data1_trn$d_data4)
      samp.data <- predict(preProcValues, samp.data)
      ## NAs cause problems in predictions
      #index of NA rows
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros
      samp.data[is.na(samp.data)] <- 0
      # predict
      data_out <- predict(meta_model, samp.data, type = "prob")
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
  setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_products")
  #setwd("D:/WHCR_spatial_predictions")
  writeRaster(gcw.mod2, filename=paste("whcr_HR_6_15_23_", i, "_.tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
  
}

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_products")

list2 <- list()
for(i in 1:24){ 
  rx <- raster(paste("whcr_HR_6_15_23_", i, "_.tif", sep=""))
  list2[[i]] <- rx
}
list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_products")
writeRaster(rast.mosaic, filename = "HR_level_prediction_7_3_23.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)
