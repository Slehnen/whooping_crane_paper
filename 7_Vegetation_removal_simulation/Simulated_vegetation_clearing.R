library(terra)
library(sp)
library(sf)

#########################################
###### load model and training data #####
#######################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_ensemble_model_fire_day.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
preProcValues <- readRDS("preProcValues_within_HR_fire_day.RDS")
testTransformed <- readRDS("testTransformed_within_HR_fire_day.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day.RDS")

data1_trn <- readRDS("trainuntrans_within_HR_fire_day.RDS")

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- rast(rasters.l)
index <- which(names(all)=="tin_1000")
names(all)[index] <- "TIN_1000"
index <- which(names(all)=="sosn_1000")
names(all)[index] <- "SOSN_1000"


index <- which(names(all) %in% names(trainTransformed))

all <- all[[index]]
names(all)

############################################################
#### Get storm surge data ###################################
############################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Source/P_surge")
surge <- st_read("al042008_2008072212_gt5.shp")
surge <- st_transform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

##########################################################################
############## process data ###############################################
##########################################################################
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
aoi <- st_read("aoi_dissolve.shp")
aoi <- st_transform(aoi, crs(all))

all <- crop(all, aoi)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
fire1_r <- all[[1]]
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r[] <- median(data1_trn$recent, na.rm=TRUE)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- rast(list(all, fire1_r, freq, surge_r, time_elap, doy))

names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"



######################################################################
######### Create model predictions ###################################
######################################################################



model.predict <- function(newdat){
  predict(meta_model$models$gbm, newdat, type = "prob", se = TRUE)[,2]
}


  samp.data <- values(all1, mat=TRUE, dataframe=TRUE)

      samp.data$chm_eucdist_3ha_clp <- quantile(data1_trn$chm_eucdist_3ha_clp, 0.5)
      names(samp.data)[1] <- "barth_dem_25"
      samp.data <- predict(preProcValues, samp.data)
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros so model will run without error, sub in NAs later
      samp.data[is.na(samp.data)] <- 0
      
      huh <- model.predict(samp.data)
      huh[index] <- NA
      
      new_suit <- all1[[1]]
      new_suit[] <- huh
   
      new_suit <- mask(new_suit, aoi)
      plot(new_suit)
      setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
      writeRaster(new_suit,  "habitat_suit_all_veg_removed.tif", overwrite=TRUE)
   
#### Original
      
      
      
      samp.data <- values(all1, mat=TRUE, dataframe=TRUE)
      
      names(samp.data)[1] <- "barth_dem_25"
      samp.data <- predict(preProcValues, samp.data)
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros so model will run without error, sub in NAs later
      samp.data[is.na(samp.data)] <- 0
      
      huh <- model.predict(samp.data)
      huh[index] <- NA
      
      new_suit <- all1[[1]]
      new_suit[] <- huh
      
      new_suit <- mask(new_suit, aoi)
      plot(new_suit)
      setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
      writeRaster(new_suit,  "habitat_suit_original.tif", overwrite=TRUE)
      
or <- rast("habitat_suit_original.tif")
veg_removed <- rast("habitat_suit_all_veg_removed.tif")

diff <- veg_removed - or
writeRaster(diff,  "habitat_suit_diff_with_veg_removed.tif", overwrite=TRUE)
      
