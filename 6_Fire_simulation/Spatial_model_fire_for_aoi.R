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
library(sf)
library(rasterVis)


#########################################
###### load model and training data #####
#######################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_ensemble_model_fire_day_5_21_23.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
preProcValues <- readRDS("preProcValues_within_HR_fire_day_5_21_23.RDS")
testTransformed <- readRDS("testTransformed_within_HR_fire_day_5_21_23.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day_5_21_23.RDS")

data1_trn <- readRDS("trainuntrans_within_HR_fire_day_5_21_23.RDS")

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
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
surge <- readOGR(dsn=getwd(), layer="al042008_2008072212_gt5")
surge <- spTransform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

##########################################################################
######### Fire data for simulation #####################################
########################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
fire1 <- readOGR(dsn=getwd(), layer="tatton_dissolve")

fire1 <- spTransform(fire1, crs(all))
surge1 <- crop(surge_r, fire1)

r <- raster(extent(fire1), res=30)
fire1_r <- rasterize(fire1, r, field=0, background=median(data1_trn$recent, na.rm=TRUE), crs = crs(all))
plot(fire1_r)

##########################################################################
############## process data ###############################################
##########################################################################

all <- crop(all, fire1)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- stack(all, fire1_r, freq, surge_r, time_elap, doy)

names(all1)[1] <- "barth_dem_25"
names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"



######################################################################
######### Create model predictions ###################################
######################################################################

model.predict <- function(newdat){
  predict(meta_model, newdat, type = "prob", se = TRUE)[,2]
}

recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
for(s in 1:length(fire1$Id)){
for(i in 1:length(recent)){
  print(s)
  print(i)
  fire1_r_30 <- reclassify(fire1_r, c(-Inf, 5, recent[i], 5, median(data1_trn$recent, na.rm=TRUE), median(data1_trn$recent, na.rm=TRUE)))
  names( fire1_r_30) <- "recent"
  all1[[23]] <- fire1_r_30 
  
  all2 <- crop(all1, fire1[s,])

  p <- data.frame(rasterToPoints(all2))

      samp.data <- p
      samp.data <- as.data.frame(samp.data)
      samp.data <- predict(preProcValues, samp.data)
      index <- rowSums(is.na(samp.data)) != 0
      # replace NAs in data frame with zeros so model will run without error, sub in NAs later
      samp.data[is.na(samp.data)] <- 0
      
      huh <- model.predict(samp.data)
      huh[index] <- NA
      
      new_suit <- all2[[1]]
      new_suit[] <- huh
   
      new_suit <- mask(new_suit, fire1[s,])
      plot(new_suit)
      setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
      writeRaster(new_suit, paste("meta_habitat_suit_", "tatton", "_", recent[i],"_new.tif", sep =""), format="GTiff", overwrite=TRUE)
   
}
}


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
rasters.l <- paste("meta_habitat_suit_", "tatton", "_", recent,"_new.tif", sep ="")


firesite1 <- stack(rasters.l)
rasterNames <- c("0 days", "30 days", "1 year", "2 years",  "3 years",
                 "4 years", "5 years", "7 year", "10 years", "15 years", "20 years")
rasterNames1 <- paste(rasterNames, " (", round(as.data.frame(cellStats(firesite1, mean))[,1], 2), ")", sep ="")
my.at <- seq(0,0.6, by = 0.025)


max_fire <- max(firesite1)
fire_diff <- firesite1-max_fire
recent <- c(1, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
m <- c(-Inf, 0, NA,  0, Inf, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
fire_days <- fire_diff
for(i in 1:nlayers(fire_diff)){
  r <- fire_diff[[i]]
  m <- c(-Inf, 0, NA,  0, Inf, recent[i])
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  y <- reclassify(r, rclmat, right=FALSE)
  fire_days[[i]] <- y
}
plot(fire_days)
sum_fire_days <- sum(fire_days, na.rm=TRUE)
fire_years <- sum_fire_days/365
fire_years[fire_years==cellStats(fire_years, max)]<- NA
fire_years[fire_years==cellStats(fire_years, min)]<- NA
plot(fire_years)

myTheme <- BTCTheme()

writeRaster(fire_years, "fire_years_tatton_new.tif", format="GTiff", overwrite=TRUE)

by_unit <- raster("fire_years_tatton_new.tif")


##########################################################################
######### Matagorda #####################################
########################################################################

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
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
surge <- readOGR(dsn=getwd(), layer="al042008_2008072212_gt5")
surge <- spTransform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
fire1 <- readOGR(dsn=getwd(), layer="matagorda_dissolve")

fire1 <- spTransform(fire1, crs(all))
surge1 <- crop(surge_r, fire1)

r <- raster(extent(fire1), res=30)
fire1_r <- rasterize(fire1, r, field=0, background=median(data1_trn$recent, na.rm=TRUE), crs = crs(all))
plot(fire1_r)

##########################################################################
############## process data ###############################################
##########################################################################

all <- crop(all, fire1)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- stack(all, fire1_r, freq, surge_r, time_elap, doy)

names(all1)[1] <- "barth_dem_25"
names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"


######################################################################
######### Create model predictions ###################################
######################################################################


recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
for(s in 1:length(fire1$Id)){
  for(i in 1:length(recent)){
    print(s)
    print(i)
    fire1_r_30 <- reclassify(fire1_r, c(-Inf, 5, recent[i], 5, median(data1_trn$recent, na.rm=TRUE), median(data1_trn$recent, na.rm=TRUE)))
    names( fire1_r_30) <- "recent"
    all1[[23]] <- fire1_r_30 
    
    all2 <- crop(all1, fire1[s,])
    
    p <- data.frame(rasterToPoints(all2))
    
    samp.data <- p
    samp.data <- as.data.frame(samp.data)
    samp.data <- predict(preProcValues, samp.data)
    index <- rowSums(is.na(samp.data)) != 0
    # replace NAs in data frame with zeros so model will run without error, sub in NAs later
    samp.data[is.na(samp.data)] <- 0
    
    huh <- model.predict(samp.data)
    huh[index] <- NA
    
    new_suit <- all2[[1]]
    new_suit[] <- huh
    
    new_suit <- mask(new_suit, fire1[s,])
    plot(new_suit)
    setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
    writeRaster(new_suit, paste("meta_habitat_suit_", "matagorda", "_", recent[i],"_new.tif", sep =""), format="GTiff", overwrite=TRUE)
    
  }
}


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
raster_list <- paste("meta_habitat_suit_", "matagorda", "_", recent,"_new.tif", sep ="")

library(rasterVis)
firesite1 <- stack(raster_list)
rasterNames <- c("0 days", "30 days", "1 year", "2 years",  "3 years",
                 "4 years", "5 years", "7 year", "10 years", "15 years", "20 years")
rasterNames1 <- paste(rasterNames, " (", round(as.data.frame(cellStats(firesite1, mean))[,1], 2), ")", sep ="")
my.at <- seq(0,0.6, by = 0.025)


##########################################################################
######### Aransas south #####################################
########################################################################

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
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
surge <- readOGR(dsn=getwd(), layer="al042008_2008072212_gt5")
surge <- spTransform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
fire1 <- readOGR(dsn=getwd(), layer="aransas_south_units_dissolve")

fire1 <- spTransform(fire1, crs(all))
surge1 <- crop(surge_r, fire1)

r <- raster(extent(fire1), res=30)
fire1_r <- rasterize(fire1, r, field=0, background=median(data1_trn$recent, na.rm=TRUE), crs = crs(all))
plot(fire1_r)

##########################################################################
############## process data ###############################################
##########################################################################

all <- crop(all, fire1)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- stack(all, fire1_r, freq, surge_r, time_elap, doy)

names(all1)[1] <- "barth_dem_25"
names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"

######################################################################
######### Create model predictions ###################################
######################################################################



recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
for(s in 1:length(fire1$Id)){
  for(i in 1:length(recent)){
    print(s)
    print(i)
    fire1_r_30 <- reclassify(fire1_r, c(-Inf, 5, recent[i], 5, median(data1_trn$recent, na.rm=TRUE), median(data1_trn$recent, na.rm=TRUE)))
    
    names( fire1_r_30) <- "recent"
    all1[[23]] <- fire1_r_30 
    
    all2 <- crop(all1, fire1[s,])
    
    p <- data.frame(rasterToPoints(all2))
    
    samp.data <- p
    samp.data <- as.data.frame(samp.data)
    samp.data <- predict(preProcValues, samp.data)
    index <- rowSums(is.na(samp.data)) != 0
    # replace NAs in data frame with zeros so model will run without error, sub in NAs later
    samp.data[is.na(samp.data)] <- 0
    
    huh <- model.predict(samp.data)
    huh[index] <- NA
    
    new_suit <- all2[[1]]
    new_suit[] <- huh
    
    new_suit <- mask(new_suit, fire1[s,])
    plot(new_suit)
    setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
    writeRaster(new_suit, paste("meta_habitat_suit_", "aransas_south", "_", recent[i],"_new.tif", sep =""), format="GTiff", overwrite=TRUE)
    
  }
}


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
rasters.l <- paste("meta_habitat_suit_", "aransas_south", "_", recent[i],"_new.tif", sep ="")

firesite1 <- stack(rasters.l)
rasterNames <- c("0 days", "30 days", "1 year", "2 years",  "3 years",
                 "4 years", "5 years", "7 year", "10 years", "15 years", "20 years")
rasterNames1 <- paste(rasterNames, " (", round(as.data.frame(cellStats(firesite1, mean))[,1], 2), ")", sep ="")
my.at <- seq(0,0.6, by = 0.025)


#########################################################################
######### Aransas north #####################################
########################################################################

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
index <- which(names(all)=="tin_1000")
names(all)[index] <- "TIN_1000"
index <- which(names(all)=="sosn_1000")
names(all)[index] <- "SOSN_1000"

index <- which(names(all)=="d_data4")
names(all)[index] <- "d_data3"

index <- which(names(all) %in% names(trainTransformed))
names(trainTransformed)
all <- all[[index]]
names(all)

############################################################
#### Get storm surge data ###################################
############################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Source/P_surge")
surge <- readOGR(dsn=getwd(), layer="al042008_2008072212_gt5")
surge <- spTransform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
fire1 <- readOGR(dsn=getwd(), layer="aransas_north_units_dissolve")

fire1 <- spTransform(fire1, crs(all))
surge1 <- crop(surge_r, fire1)

r <- raster(extent(fire1), res=30)
fire1_r <- rasterize(fire1, r, field=0, background=median(data1_trn$recent, na.rm=TRUE), crs = crs(all))
plot(fire1_r)

##########################################################################
############## process data ###############################################
##########################################################################

all <- crop(all, fire1)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- stack(all, fire1_r, freq, surge_r, time_elap, doy)

names(all1)[1] <- "barth_dem_25"
names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"

######################################################################
######### Create model predictions ###################################
######################################################################


recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
for(s in 1:length(fire1$Id)){
  for(i in 1:length(recent)){
    print(s)
    print(i)
    fire1_r_30 <- reclassify(fire1_r, c(-Inf, 5, recent[i], 5, median(data1_trn$recent, na.rm=TRUE), median(data1_trn$recent, na.rm=TRUE)))
    names( fire1_r_30) <- "recent"
    all1[[23]] <- fire1_r_30 
    
    all2 <- crop(all1, fire1[s,])
    
    p <- data.frame(rasterToPoints(all2))
    
    samp.data <- p
    samp.data <- as.data.frame(samp.data)
    samp.data$age_Adt <- 1
    samp.data$age_Juv <- 0
    samp.data$age_Sub <- 0
    samp.data <- predict(preProcValues, samp.data)
    index <- rowSums(is.na(samp.data)) != 0
    # replace NAs in data frame with zeros so model will run without error, sub in NAs later
    samp.data[is.na(samp.data)] <- 0
    
    huh <- model.predict(samp.data)
    huh[index] <- NA
    
    new_suit <- all2[[1]]
    new_suit[] <- huh
    
    new_suit <- mask(new_suit, fire1[s,])
    plot(new_suit)
    setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
    writeRaster(new_suit, paste("meta_habitat_suit_", "aransas_north", "_", recent[i],"_new.tif", sep =""), format="GTiff", overwrite=TRUE)
    
  }
}


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
rasters.l <- list.files(pattern = "\\.tif$") 
index <- grep("habitat_suit_aransas_north", rasters.l)
rasters.l <- rasters.l[index]

firesite1 <- stack(rasters.l[1], rasters.l[6], rasters.l[7], rasters.l[10], rasters.l[2],
                   rasters.l[3], rasters.l[4], rasters.l[5], rasters.l[8], rasters.l[9])
rasterNames <- c("0 days", "30 days", "1 year", "2 years", "20 years", "3 years",
                 "4 years", "5 years", "7 year", "10 years", "15 years")
rasterNames1 <- paste(rasterNames, " (", round(as.data.frame(cellStats(firesite1, mean))[,1], 2), ")", sep ="")
my.at <- seq(0,0.6, by = 0.025)


#########################################################################
######### east units #####################################
########################################################################

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
index <- which(names(all)=="tin_1000")
names(all)[index] <- "TIN_1000"
index <- which(names(all)=="sosn_1000")
names(all)[index] <- "SOSN_1000"

index <- which(names(all)=="d_data4")
names(all)[index] <- "d_data3"

index <- which(names(all) %in% names(trainTransformed))

all <- all[[index]]
names(all)

############################################################
#### Get storm surge data ###################################
############################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Source/P_surge")
surge <- readOGR(dsn=getwd(), layer="al042008_2008072212_gt5")
surge <- spTransform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
fire1 <- readOGR(dsn=getwd(), layer="east_units_dissolve")

fire1 <- spTransform(fire1, crs(all))
surge1 <- crop(surge_r, fire1)

r <- raster(extent(fire1), res=30)
fire1_r <- rasterize(fire1, r, field=0, background=median(data1_trn$recent, na.rm=TRUE), crs = crs(all))
plot(fire1_r)
##########################################################################
############## process data ###############################################
##########################################################################

all <- crop(all, fire1)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- stack(all, fire1_r, freq, surge_r, time_elap, doy)

names(all1)[1] <- "barth_dem_25"
names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"

######################################################################
######### Create model predictions ###################################
######################################################################



recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
for(s in 1:length(fire1$Id)){
  for(i in 1:length(recent)){
    print(s)
    print(i)
    fire1_r_30 <- reclassify(fire1_r, c(-Inf, 5, recent[i], 5, median(data1_trn$recent, na.rm=TRUE), median(data1_trn$recent, na.rm=TRUE)))
    names( fire1_r_30) <- "recent"
    all1[[23]] <- fire1_r_30 
    
    all2 <- crop(all1, fire1[s,])
    
    p <- data.frame(rasterToPoints(all2))
    
    samp.data <- p
    samp.data <- as.data.frame(samp.data)
    samp.data$age_Adt <- 1
    samp.data$age_Juv <- 0
    samp.data$age_Sub <- 0
    samp.data <- predict(preProcValues, samp.data)
    index <- rowSums(is.na(samp.data)) != 0
    # replace NAs in data frame with zeros so model will run without error, sub in NAs later
    samp.data[is.na(samp.data)] <- 0
    
    huh <- model.predict(samp.data)
    huh[index] <- NA
    
    new_suit <- all2[[1]]
    new_suit[] <- huh
    
    new_suit <- mask(new_suit, fire1[s,])
    plot(new_suit)
    setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
    writeRaster(new_suit, paste("meta_habitat_suit_", "aransas_east", "_", recent[i],"_new.tif", sep =""), format="GTiff", overwrite=TRUE)
    
  }
}

#########################################################################
######### leftover_units #####################################
########################################################################

########################################################################
##### load prediction layers ###############################################
#######################################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/HR_level_and_within_HR_level_rasters_for_prediction_30m")
rasters.l <- list.files(pattern = "\\.tif$") 
all <- raster::stack(rasters.l)
index <- which(names(all)=="tin_1000")
names(all)[index] <- "TIN_1000"
index <- which(names(all)=="sosn_1000")
names(all)[index] <- "SOSN_1000"

index <- which(names(all)=="d_data4")
names(all)[index] <- "d_data3"

index <- which(names(all) %in% names(trainTransformed))

all <- all[[index]]
names(all)

############################################################
#### Get storm surge data ###################################
############################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Source/P_surge")
surge <- readOGR(dsn=getwd(), layer="al042008_2008072212_gt5")
surge <- spTransform(surge, crs(all))
surge_r <- rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
fire1 <- readOGR(dsn=getwd(), layer="leftovers_Dissolve")

fire1 <- spTransform(fire1, crs(all))
surge1 <- crop(surge_r, fire1)

r <- raster(extent(fire1), res=30)
fire1_r <- rasterize(fire1, r, field=0, background=median(data1_trn$recent, na.rm=TRUE), crs = crs(all))
plot(fire1_r)

##########################################################################
############## process data ###############################################
##########################################################################

all <- crop(all, fire1)
#all <- mask(all, fire1)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
fire1_r <- resample(fire1_r, all[[1]]) # time since most recent fire
surge_r <- resample(surge_r, all[[1]]) # storm surge impact
time_elap <- resample(time_elap, all[[1]]) # time since storm surge
doy <- fire1_r
doy[] <- median(data1_trn$doy)

all1 <- stack(all, fire1_r, freq, surge_r, time_elap, doy)

names(all1)[1] <- "barth_dem_25"
names(all1)[23] <- "recent"
names(all1)[24] <- "freq"
names(all1)[25] <- "surge"
names(all1)[26] <- "time_elapsed"
names(all1)[27] <- "doy"

######################################################################
######### Create model predictions ###################################
######################################################################



recent <- c(0, 30, 365, 365*2, 365*3, 365*4, 5*365, 365*7, 365*10, 365*15, 365*20)
for(s in 1:length(fire1$Shape_Leng)){
  for(i in 1:length(recent)){
    print(s)
    print(i)
    fire1_r_30 <- reclassify(fire1_r, c(-Inf, 5, recent[i], 5, median(data1_trn$recent, na.rm=TRUE), median(data1_trn$recent, na.rm=TRUE)))
    names( fire1_r_30) <- "recent"
    all1[[23]] <- fire1_r_30 
    
    all2 <- crop(all1, fire1[s,])
    
    p <- data.frame(rasterToPoints(all2))
    
    samp.data <- p
    samp.data <- as.data.frame(samp.data)
    samp.data$age_Adt <- 1
    samp.data$age_Juv <- 0
    samp.data$age_Sub <- 0
    samp.data <- predict(preProcValues, samp.data)
    index <- rowSums(is.na(samp.data)) != 0
    # replace NAs in data frame with zeros so model will run without error, sub in NAs later
    samp.data[is.na(samp.data)] <- 0
    
    huh <- model.predict(samp.data)
    huh[index] <- NA
    
    new_suit <- all2[[1]]
    new_suit[] <- huh
    
    new_suit <- mask(new_suit, fire1[s,])
    plot(new_suit)
    setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
    writeRaster(new_suit, paste("meta_habitat_suit_", "leftover_units", "_", recent[i],"_new.tif", sep =""), format="GTiff", overwrite=TRUE)
    
  }
}




setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/simulation")
rasters.l <- list.files(pattern = "\\.tif$") 
index <- grep("habitat_suit_aransas_east", rasters.l)
rasters.l <- rasters.l[index]

library(rasterVis)
firesite1 <- stack(rasters.l[1], rasters.l[6], rasters.l[7], rasters.l[10], rasters.l[2],
                   rasters.l[3], rasters.l[4], rasters.l[5], rasters.l[8], rasters.l[9])
rasterNames <- c("0 days", "30 days", "1 year", "2 years", "20 years", "3 years",
                 "4 years", "5 years", "7 year", "10 years", "15 years")
rasterNames1 <- paste(rasterNames, " (", round(as.data.frame(cellStats(firesite1, mean))[,1], 2), ")", sep ="")
my.at <- seq(0,0.6, by = 0.025)

