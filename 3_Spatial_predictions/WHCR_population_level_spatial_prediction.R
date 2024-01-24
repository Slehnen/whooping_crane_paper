library(terra)
library(sp)
library(xgboost)

######################################################
##################### load models#####################
######################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("pop_level_meta_model.RDS")
preProcValues <- readRDS("preProcValues_population.RDS")
testdata <- readRDS("testTransformed_population.RDS")
traindata <- readRDS("trainTransformed_population.RDS" )
data1_trn <- readRDS("untrans_population.RDS")
data.smote <- readRDS("smote_population.RDS")

########################################################################
##### Spatial prediction ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")

rasters.l <- list.files(pattern = "\\.tif$") 
all <- rast(rasters.l)
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
  alli <- terra::crop(x = all,y = terra::ext(xseq[i], xseq[i+1], 7043218, 7240138))
  print("crop done")
  p <- terra::values(alli)
  p <- data.frame(p)
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
  
  template_ras <- alli[[1]]
  values(template_ras) <- data_out
  
  setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/pop_level_products")
  writeRaster(template_ras, filename=paste("whcr_pop", i, "_.tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/pop_level_products")

list2 <- list()
for(i in 1:24){ 
  rx <- raster(paste("whcr_pop", i, "_.tif", sep=""))
  list2[[i]] <- rx
}

list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/pop_level_products")
writeRaster(rast.mosaic, filename = "population_level_prediction.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)