library(vip)
library(caret)
library(gbm)
library("DALEX")
library("randomForest")
library(dplyr)
library(forcats)
library(ggplot2)
library(ggthemes)

########################################################
# Population ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("pop_level_meta_model_5_30_23.RDS")
preProcValues <- readRDS("preProcValues_population_5_30_23.RDS")
testdata <- readRDS("testTransformed_population_5_30_23.RDS")
trainTransformed <- readRDS("trainTransformed_population_5_30_23.RDS" )
data1_trn <- readRDS("untrans_population_5_30_23.RDS")

y <- testdata$pop_data
y <- as.character(y)
y[y == "Neg"] <- 1
y[y == "Pos"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}


explainer_en <- DALEX::explain(model = meta_model, 
                               data = testdata[,-17],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")


model_parts_ensemble_pop <- model_parts(explainer_en, type = "variable_importance", 
                                        N = 5000, B = 100)
head(model_parts_ensemble_pop, 8)
plot(model_parts_ensemble_pop)

model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "d_data"] <- "Palmer's Drought Severity Index"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "baren_1000"] <- "% barren (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "forest_1000"] <- "% forest (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "shrub_1000"] <- "% shrubland (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "water_1000"] <- "% open water (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "pal_aq_bed_4000"] <- "% palustrine aquatic bed (4km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "est_emer_wet_8000"] <- "% estuarine emergent wetland (8km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "grass_8000"] <- "% grassland (8km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "pal_aq_bed_8000"] <- "% palustrine aquatic bed (8km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "algal_flats_1000"] <- "% algal flats (8km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "deep_sand_grass_swale_marsh_1000"] <- "% deep sand grass swale marsh (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "dune_coast_grass_1000"] <- "% dune coast grassland (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "salt_tidal_high_marsh_1000"] <- "% salt tidal high marsh (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "salt_tidal_marsh_1000"] <- "% salt tidal marsh (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "tin_1000"] <- "Time Integrated NDVI (1km)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "maxn_1000"] <- "Maximum NDVI (1km)"

plot(model_parts_ensemble_pop)
parm_keep <- c(unique(model_parts_ensemble_pop$variable))

model_parts_ensemble_pop1 <- as.data.frame(model_parts_ensemble_pop)
model_parts_ensemble_pop1$variable <- factor(model_parts_ensemble_pop1$variable)
model_parts_ensemble_pop2 <- subset(model_parts_ensemble_pop1, variable %in% parm_keep)

ggplot_imp <- function(dataframe, model_parts) {
  metric_name <- attr(model_parts[[1]], "loss_name")
  metric_lab <- paste(
    "One minus AUC loss after permutations")
  
  full_vip <- bind_rows(dataframe) %>%
    filter(variable != "_baseline_")
  
  perm_vals <- full_vip %>% 
    filter(variable == "_full_model_") %>% 
    group_by(label) %>% 
    summarise(dropout_loss = mean(dropout_loss))
  
  p <- full_vip %>%
    filter(variable != "_full_model_") %>% 
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable)) 
  if(length(dataframe) > 1) {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(alpha = 0.2)
  } else {
    p <- p + 
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)
    
  }
  p +
    theme(legend.position = "none") +
    theme_few()+
    labs(x = metric_lab, 
         y = NULL,  fill = NULL,  color = NULL)
}

ggplot_imp(model_parts_ensemble_pop2, model_parts_ensemble_pop)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_pop2, "pop_var_dat_ind.RDS")
saveRDS(model_parts_ensemble_pop, "model_parts_ensemble_pop_ind.RDS")

model_parts_ensemble_pop2 <- readRDS("pop_var_dat_ind.RDS")
model_parts_ensemble_pop <- readRDS("model_parts_ensemble_pop_ind.RDS")

ggplot_imp(model_parts_ensemble_pop2, model_parts_ensemble_pop)

########################################################
# HR ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("HR_level_meta_model_5_18_2023.RDS")
preProcValues <- readRDS("preProcValues_HR_5_18_23.RDS")
data1_trn <- readRDS("data1_trn_HR_5_18_23.RDS")
testTransformed <- readRDS("testTransformed_HR_5_18_23.RDS")
trainTransformed <- readRDS("trainTransformed_HR_5_18_23.RDS")


y <- testTransformed$type
y <- as.character(y)
y[y == "random"] <- 1
y[y == "used"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

explainer_en <- DALEX::explain(model = meta_model, 
                               data = testTransformed[,-11],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")


model_parts_ensemble_HR <- model_parts(explainer_en, type = "variable_importance",
                                       N = 5000, B = 100)
head(model_parts_ensemble_HR, 8)
plot(model_parts_ensemble_HR)

model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "d_data4"] <- "Palmer's Drought Severity Index (4km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "veg_ht_8000"] <- "Vegetation height (8km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "salt_tidal_marsh_500"] <- "% salt tidal marsh (500m)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "salt_tidal_marsh_4000"] <- "% salt tidal marsh (4km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "dune_coast_grass_4000"] <- "% dune coast grass (4km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "deep_sand_shrub_2000"] <- "% deep sand shrub (2km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "EOST_4000"] <- "End of season day (4km))"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "deep_sand_live_oak_marsh_4000"] <- "% deep sand live oak marsh (4km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "TIN_4000"] <- "Time Integrated NDVI (4km)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "SOSN_4000"] <- "Start of Season NDVI (4km)"


plot(model_parts_ensemble_HR)
parm_keep <- c(unique(model_parts_ensemble_HR$variable))

model_parts_ensemble_HR1 <- as.data.frame(model_parts_ensemble_HR)
model_parts_ensemble_HR1$variable <- factor(model_parts_ensemble_HR1$variable)
model_parts_ensemble_HR2 <- subset(model_parts_ensemble_HR1, variable %in% parm_keep)

ggplot_imp(model_parts_ensemble_HR2, model_parts_ensemble_HR)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_HR2, "HR_var_dat_ind.RDS")
saveRDS(model_parts_ensemble_HR, "model_parts_ensemble_HR_ind.RDS")

model_parts_ensemble_HR2 <- readRDS("HR_var_dat_ind.RDS")
model_parts_ensemble_HR <- readRDS("model_parts_ensemble_HR_ind.RDS")

ggplot_imp(model_parts_ensemble_HR2, model_parts_ensemble_HR)

##########################################################
# Roost #################################################
###########################################################


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_meta_model_roost_5_18_23.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
preProcValues <- readRDS("preProcValues_within_HR_roosting_5_18_23.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_roosting_5_18_23.RDS")
testTransformed <- readRDS("testTransformed_within_HR_roosting_5_18_23.RDS")
data1_trn <- readRDS("trainuntrans_within_HR_roosting_5_18_23.RDS")

y <- testTransformed$type
y <- as.character(y)
y[y == "random"] <- 1
y[y == "used"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}




explainer_en <- DALEX::explain(model = meta_model, 
                               data = testTransformed[,-27],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")


model_parts_ensemble_roost <- model_parts(explainer_en, type = "variable_importance",
                                          N = 5000, b = 100)
head(model_parts_ensemble_roost, 8)
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "TIN_1000"] <- "Time Integrated NDVI (1km)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "DUR_500"] <- "Duration of active NDVI (500m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "pal_shrub_wet_500"] <- "% palustrine shrub wetland (500m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "riparian_herb_veg_500"] <- "% riparian herbaceous vegetation (500m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "algal_flats_125"] <- "% algal flats (125m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "est_aq_bed_500"] <- "% estuarine aquatic bed (500m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "pal_emer_wet_50"] <- "% palustrine emergent wetland (50m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "water_1000"] <- "% open water (1km)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "grass_125"] <- "% grassland (125m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "SOSN_1000"] <- "Start of Season NDVI (1km)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "chm_eucdist_3ha_clp"] <- "Distance from nearest shrub/tree clump"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "developed_1000"] <- "% developed (1km)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "baren_50"] <- "% barren (50m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "ox_daisy_flats_500"] <- "% ox daisy flats (500m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "surge"] <- "Storm surge impact"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "doy"] <- "Day of year"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "veg_ht_25"] <- "Vegetation height (25m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "time_elapsed"] <- "Time since most recent storm surge"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "tpi_data"] <- "Topographic Positon Index (250m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "water_50"] <- "% open water (50m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "salt_tidal_marsh_50"] <- "% salt tidal marsh (50m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "tidal_flats_125"] <- "% tidal flats (125m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "salt_tidal_high_marsh_50"] <- "% salt tidal high marsh (50m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "riparian_herb_veg_125"] <- "% riparian herbaceous vegetation (125m)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "grass_1000"] <- "% grassland (1km)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "barth_dem_25"] <- "Elevation (25m)"
parm_keep <- c(unique(model_parts_ensemble_roost$variable))

model_parts_ensemble_roost1 <- as.data.frame(model_parts_ensemble_roost)
model_parts_ensemble_roost1$variable <- factor(model_parts_ensemble_roost1$variable)
model_parts_ensemble_roost2 <- subset(model_parts_ensemble_roost1, variable %in% parm_keep)

ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_roost2, "Roost_var_dat_ind.RDS")
saveRDS(model_parts_ensemble_roost, "model_parts_ensemble_roost_ind.RDS")
model_parts_ensemble_roost2 <- readRDS("Roost_var_dat_ind.RDS")
model_parts_ensemble_roost <- readRDS("model_parts_ensemble_roost_ind.RDS")
ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost)

########################################################
# within HR - day ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_ensemble_model_fire_day_5_21_23.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
preProcValues <- readRDS("preProcValues_within_HR_fire_day_5_21_23.RDS")
testTransformed <- readRDS("testTransformed_within_HR_fire_day_5_21_23.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day_5_21_23.RDS")

y <- testTransformed$type
y <- as.character(y)
y[y == "random"] <- 1
y[y == "used"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

explainer_en <- DALEX::explain(model = meta_model, 
                               data = testTransformed[,-28],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")


model_parts_ensemble_day <- model_parts(explainer_en, type = "variable_importance",
                                        N = 5000, B = 100)
head(model_parts_ensemble_day, 12)
plot(model_parts_ensemble_day)

model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "TIN_1000"] <- "Time Integrated NDVI (1km)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "DUR_500"] <- "Duration of active NDVI (500m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "pal_shrub_wet_500"] <- "% palustrine shrub wetland (500m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "riparian_herb_veg_500"] <- "% riparian herbaceous vegetation (500m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "algal_flats_125"] <- "% algal flats (125m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "est_aq_bed_500"] <- "% estuarine aquatic bed (500m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "pal_emer_wet_50"] <- "% palustrine emergent wetland (50m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "water_1000"] <- "% open water (1km)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "grass_125"] <- "% grassland (125m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "SOSN_1000"] <- "Start of Season NDVI (1km)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "chm_eucdist_3ha_clp"] <- "Distance from nearest shrub/tree clump"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "developed_1000"] <- "% developed (1km)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "baren_50"] <- "% barren (50m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "ox_daisy_flats_500"] <- "% ox daisy flats (500m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "surge"] <- "Storm surge impact"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "doy"] <- "Day of year"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "veg_ht_25"] <- "Vegetation height (25m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "time_elapsed"] <- "Time since most recent storm surge"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "tpi_data"] <- "Topographic Position Index (250m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "water_50"] <- "% open water (50m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "salt_tidal_marsh_50"] <- "% salt tidal marsh (50m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "tidal_flats_125"] <- "% tidal flats (125m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "salt_tidal_high_marsh_50"] <- "% salt tidal high marsh (50m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "riparian_herb_veg_125"] <- "% riparian herbaceous vegetation (125m)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "grass_1000"] <- "% grassland (1km)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "barth_dem_25"] <- "Elevation (25m)"
parm_keep <- c(unique(model_parts_ensemble_day$variable))

model_parts_ensemble_day1 <- as.data.frame(model_parts_ensemble_day)
model_parts_ensemble_day1$variable <- factor(model_parts_ensemble_day1$variable)
model_parts_ensemble_day2 <- subset(model_parts_ensemble_day1, variable %in% parm_keep)

ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_day2, "Day_var_dat_group_ind.RDS")
saveRDS(model_parts_ensemble_day, "model_parts_ensemble_day_group_ind.RDS")
model_parts_ensemble_day2 <- readRDS("Day_var_dat_group_ind.RDS")
model_parts_ensemble_day <- readRDS("model_parts_ensemble_day_group_ind.RDS")
ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day)
