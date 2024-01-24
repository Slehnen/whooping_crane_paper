library(sp)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(tidyterra)
library(terra)

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
surge <- st_transform(surge, st_crs(all))
surge_r <- terra::rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

##########################################################################
############## Clip to Estuarine Emergent Wetland example ###############################################
##########################################################################
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
aoi <- st_read("Estuarine Emergent Wetland example.shp")
aoi <- st_transform(aoi, st_crs(all))

all <- terra::crop(all, aoi)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <-  3 # median(data1_trn$freq)
surge_r <- terra::resample(surge_r, all[[1]]) # storm surge impact
time_elap <- terra::resample(time_elap, all[[1]]) # time since storm surge
doy <-  all[[1]]
doy[] <- median(data1_trn$doy)


fire_recent <- c(1, 30, 365, 365*5, 365*15)
for(i in 1:length(fire_recent)){
print(i)
fire1_r <- all[[1]]
fire1_r[] <- fire_recent[i]

all1 <- c(all, fire1_r, freq, surge_r, time_elap, doy)

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
      p <- terra::values(all1)
      samp_data <- data.frame(p)
      samp_data$chm_eucdist_3ha_clp <- quantile(data1_trn$chm_eucdist_3ha_clp, 0.5)
      names(samp_data)[1] <- "barth_dem_25"
      samp_data <- predict(preProcValues, samp_data)
      index <- rowSums(is.na(samp_data)) != 0
      # replace NAs in data frame with zeros so model will run without error, sub in NAs later
      samp_data[is.na(samp_data)] <- 0
      huh <- model.predict(samp_data)
      huh[index] <- NA

      template_ras <- all1[[1]]
      values(template_ras) <- huh
      new_suit <- terra::mask(template_ras, aoi)
      plot(new_suit)
      setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/Example_fires/Predictions")
      writeRaster(new_suit,  paste("EEW_", fire_recent[i], "_days_after_fire.tif", sep =""), overwrite=TRUE)
}      
##########################################################################
############## Clip to Palustrine Emergent Wetland example ###############################################
##########################################################################
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

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Source/P_surge")
surge <- st_read("al042008_2008072212_gt5.shp")
surge <- st_transform(surge, st_crs(all))
surge_r <- terra::rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
aoi <- st_read("Palustrine Emergent Wetland.shp")
aoi <- st_transform(aoi, st_crs(all))

all <- terra::crop(all, aoi)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <- median(data1_trn$freq)
surge_r <- terra::resample(surge_r, all[[1]]) # storm surge impact
time_elap <- terra::resample(time_elap, all[[1]]) # time since storm surge
doy <- all[[1]]
doy[] <- median(data1_trn$doy)

fire_recent <- c(1, 30, 365, 365*5, 365*15)
for(i in 1:length(fire_recent)){
  print(i)
  fire1_r <- all[[1]]
  fire1_r[] <- fire_recent[i]
  
  all1 <- c(all, fire1_r, freq, surge_r, time_elap, doy)
  
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
  p <- terra::values(all1)
  samp_data <- data.frame(p)
  samp_data$chm_eucdist_3ha_clp <- quantile(data1_trn$chm_eucdist_3ha_clp, 0.5)

  names(samp_data)[1] <- "barth_dem_25"
  samp_data <- predict(preProcValues, samp_data)
  index <- rowSums(is.na(samp_data)) != 0
  # replace NAs in data frame with zeros so model will run without error, sub in NAs later
  samp_data[is.na(samp_data)] <- 0
  
  huh <- model.predict(samp_data)
  huh[index] <- NA
  template_ras <- all1[[1]]
  values(template_ras) <- huh
  new_suit <- terra::mask(template_ras, aoi)
  
  plot(new_suit)
  setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/Example_fires/Predictions")
  writeRaster(new_suit,  paste("PEW_", fire_recent[i], "_days_after_fire.tif", sep =""), overwrite=TRUE)
  }  

################################################################################
### Grassland ##################################################################
################################################################################

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
surge <- st_transform(surge, st_crs(all))
surge_r <- terra::rasterize(x = surge, all[[1]], field = "ProbSurge05", background = 0)

##########################################################################
############## Clip to Grassland example ###############################################
##########################################################################
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
aoi <- st_read("Grassland example.shp")
aoi <- st_transform(aoi, st_crs(all))

all <- terra::crop(all, aoi)
time_elap <- all[[1]]
time_elap[] <- median(data1_trn$time_elapsed, na.rm=TRUE)
freq <- all[[1]]
freq[] <-  3 # median(data1_trn$freq)
surge_r <- terra::resample(surge_r, all[[1]]) # storm surge impact
time_elap <- terra::resample(time_elap, all[[1]]) # time since storm surge
doy <-  all[[1]]
doy[] <- median(data1_trn$doy)

fire_recent <- c(1, 30, 365, 365*5, 365*15)
for(i in 1:length(fire_recent)){
  print(i)
  fire1_r <- all[[1]]
  fire1_r[] <- fire_recent[i]
  
  all1 <- c(all, fire1_r, freq, surge_r, time_elap, doy)
  
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
  p <- terra::values(all1)
  samp_data <- data.frame(p)
  samp_data$chm_eucdist_3ha_clp <- quantile(data1_trn$chm_eucdist_3ha_clp, 0.5)
  
  names(samp_data)[1] <- "barth_dem_25"
  samp_data <- predict(preProcValues, samp_data)
  index <- rowSums(is.na(samp_data)) != 0
  # replace NAs in data frame with zeros so model will run without error, sub in NAs later
  samp_data[is.na(samp_data)] <- 0
  
  huh <- model.predict(samp_data)
  huh[index] <- NA
  
  template_ras <- all1[[1]]
  values(template_ras) <- huh
  new_suit <- terra::mask(template_ras, aoi)
  
  plot(new_suit)
  setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/Example_fires/Predictions")
  writeRaster(new_suit,  paste("Grass_", fire_recent[i], "_days_after_fire.tif", sep =""), overwrite=TRUE)
}    


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/Example_fires/Predictions")

rasters.l <- list.files(pattern = "\\.tif$")
library(gtools)
rasters.l <- mixedsort(sort(rasters.l))
EEW <- terra::rast(rasters.l[1:5])

library(tidyterra)
library(terra)

names(EEW) <- c("1 day", "30 days", "1 year", "5 years", "15 years")
mean_values_EEW <- global(EEW, "mean", na.rm=TRUE)
mean_values_EEW$label <- rownames(mean_values_EEW)
mean_values_EEW$label <- factor(mean_values_EEW$label, levels = c("1 day", "30 days", "1 year", "5 years", "15 years"))
EEW_bar <- ggplot(mean_values_EEW, aes(x= label, y = mean))+ 
  geom_bar(stat = "identity")+
  theme_few(10)+
  ylim(c(0, 0.3))+
  ylab("")+
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())
EEW_bar
eew <- ggplot() +
  geom_spatraster(data = EEW)+
  facet_wrap(~lyr, ncol = 5) +
  scale_fill_whitebox_c(
    palette = "viridi",
    labels = scales::label_number(),
    breaks = seq(0, 0.6, by =0.2),
    limits = c(0, 0.6),
    name="predicted value"
  ) +
  theme_void(15)+
  labs(title = "99% estuarine emergent wetland")+
  theme(strip.clip = "off")+
  theme(legend.position='none')+
  theme(plot.margin = unit(c(0,0,0,0), "cm")) # ("left", "right", "bottom", "top")
eew

# PEW
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/Example_fires/Predictions")

rasters.l <- list.files(pattern = "\\.tif$")
library(gtools)
rasters.l <- mixedsort(sort(rasters.l))
index <- grepl(x = rasters.l, pattern = "PEW")
rasters.l <- rasters.l[index]
PEW <- terra::rast(rasters.l)

library(tidyterra)
library(terra)

names(PEW) <- c("1 day", "30 days", "1 year", "5 years", "15 years")
mean_values_PEW <- global(PEW, "mean", na.rm=TRUE)
mean_values_PEW$label <- rownames(mean_values_PEW)
mean_values_PEW$label <- factor(mean_values_PEW$label, levels = c("1 day", "30 days", "1 year", "5 years", "15 years"))
PEW_bar <- ggplot(mean_values_PEW, aes(x= label, y = mean))+ 
  geom_bar(stat = "identity")+
  theme_few(10)+
  ylim(c(0, 0.3))+
  ylab("")+
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())
PEW_bar

pew <- ggplot() +
  geom_spatraster(data = PEW)+
  facet_wrap(~lyr, ncol = 5) +
  labs(color = "Users By labs")+
  scale_fill_whitebox_c(
    palette = "viridi",
    labels = scales::label_number(),
    breaks = seq(0, 0.6, by =0.2),
    limits = c(0, 0.6),
    name="Predicted resource selection propensity "
  ) +
  theme_void(15)+
  labs(title = "77% palustrine wetland cover")+
  theme(strip.clip = "off")+
  theme(legend.position="bottom")+
  theme(plot.margin = unit(c(0,0,0,0), "cm")) # ("left", "right", "bottom", "top")
pew

# Extract the legend. Returns a gtable
leg <- ggpubr::get_legend(pew)
pew <- pew + theme(legend.position='none')

# Convert to a ggplot and print
leg <- as_ggplot(leg)

# GRASS
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/Example_fires/Predictions")
rasters.l <- list.files(pattern = "\\.tif$")
rasters.l <- mixedsort(sort(rasters.l))
index <- grepl(x = rasters.l, pattern = "Grass")
rasters.l <- rasters.l[index]
GRASS <- terra::rast(rasters.l)
names(GRASS) <- c("1 day", "30 days", "1 year", "5 years", "15 years")
library(gtools)

mean_values_GRASS <- global(GRASS, "mean", na.rm=TRUE)

mean_values_GRASS <- global(GRASS, "mean", na.rm=TRUE)
mean_values_GRASS$label <- rownames(mean_values_GRASS)
mean_values_GRASS$label <- factor(mean_values_GRASS$label, levels = c("1 day", "30 days", "1 year", "5 years", "15 years"))
GRASS_bar <- ggplot(mean_values_GRASS, aes(x= label, y = mean))+ 
  geom_bar(stat = "identity")+
  theme_few(10)+
  ylim(c(0, 0.3))+
  ylab("")+
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank())
GRASS_bar

grass <- ggplot() +
  geom_spatraster(data = GRASS)+
  facet_wrap(~lyr, ncol = 5) +
  scale_fill_whitebox_c(
    palette = "viridi",
    labels = scales::label_number(),
    breaks = seq(0, 0.6, by =0.2),
    limits = c(0, 0.6),
    name="predicted value"
  ) +
  theme_void(15)+
  labs(title = "82% grassland/herbaceous cover") +
  theme(strip.clip = "off")+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+ # ("left", "right", "bottom", "top")
theme(legend.position='none')
grass

grid_plot_fire <- cowplot::plot_grid(eew + theme(plot.margin = unit(c(0, 0, 0, 1), "cm")), 
                   EEW_bar + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")), 
                   grass+ theme(plot.margin = unit(c(0, 0, 0, 1), "cm")),
                   GRASS_bar+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                   pew+ theme(plot.margin = unit(c(0, 0, 0, 1), "cm")),
                   PEW_bar+ theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                   leg + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                   labels = "", 
                   rel_heights = c(1, 0.3, 1, 0.3, 1, 0.3,0.5), 
                   ncol = 1,
                   align = "v",
                   axis = "b", 
                   rel_widths = c(1,1,1,1,1,1,0.55))
grid_plot_fire

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pubs_final")

# ragg always works for mac
ragg::agg_tiff("Figure_4_Whooping_crane_fire_examples.tiff", width = 5.5, height = 8, units = "in", res = 450)
grid_plot_fire
dev.off()

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pubs_final")
jpeg(filename = "Figure_4_Whooping_crane_fire_examples.jpeg", width = 4000, height = 6000,
     pointsize = 12, quality = 100, bg = "white", res = 450)
grid_plot_fire
dev.off()