library(terra)
library(sp)
library(xgboost)

######################################################
##################### Random data generation #########
######################################################


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("HR_level_meta_model.RDS")
preProcValues <- readRDS("preProcValues_HR.RDS")
data1_trn <- readRDS("data1_trn_HR.RDS")
testTransformed <- readRDS("testTransformed_HR.RDS")
trainTransformed <- readRDS("trainTransformed_HR.RDS")


########################################################################
##### Spatial prediction ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- rast(rasters.l)

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

for(i in 1:24){
  print(i)
  alli <- terra::crop(x = all,y = terra::ext(xseq[i], xseq[i+1], 7043218, 7240138))
  print("crop done")
  p <- terra::values(alli)
  p <- data.frame(p)
  print("raster to points done")
  if(dim(p)[1]==0)next
  
  chunk1 <- seq(1, length(p[,1]), by=1000)
  startTime=date()
  result.1 <-
    foreach(a=chunk1)%dopar% {
      samp.data <- p[a:(a+999),]
      samp.data <- as.data.frame(samp.data)
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
  
  template_ras <- alli[[1]]
  values(template_ras) <- data_out
  setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_products")
  writeRaster(template_ras, filename=paste("whcr_HR_", i, "_.tif", sep=""),format="GTiff",datatype="FLT4S", overwrite=TRUE)
}

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_products")

list2 <- list()
for(i in 1:24){ 
  rx <- raster(paste("whcr_HR_", i, "_.tif", sep=""))
  list2[[i]] <- rx
}
list2$fun   <- max
rast.mosaic <- do.call(mosaic,list2)
plot(rast.mosaic, axes = FALSE, legend = TRUE, bty = "n", box = FALSE)

crs(rast.mosaic) <- crs(rx)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_products")
writeRaster(rast.mosaic, filename = "HR_level_prediction.tif",
            format = "GTiff", datatype="FLT4S", overwrite = TRUE)
