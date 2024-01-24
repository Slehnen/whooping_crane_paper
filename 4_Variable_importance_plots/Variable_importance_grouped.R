library(gbm)
library(DALEX)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(forcats)

`%notin%` <- Negate(`%in%`)

########################################################
# Population ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("pop_level_meta_model.RDS")
preProcValues <- readRDS("preProcValues_population.RDS")
testdata <- readRDS("testTransformed_population.RDS")
trainTransformed <- readRDS("trainTransformed_population.RDS" )
data1_trn <- readRDS("untrans_population.RDS")

y <- testdata$pop_data
y <- as.character(y)
y[y == "Neg"] <- 1
y[y == "Pos"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

drought <- list(c("d_data"))
ccap <- list(c("baren_1000", "forest_1000", "est_emer_wet_8000", "grass_8000", "pal_aq_bed_4000", "pal_aq_bed_8000", "shrub_1000", "water_1000"))
tems <- list(c("salt_tidal_high_marsh_1000", "salt_tidal_marsh_1000", "deep_sand_grass_swale_marsh_1000", 
               "algal_flats_1000", "dune_coast_grass_1000"))
phen <- list(c("tin_1000", "maxn_1000"))

grouped_vars <- c(drought, ccap, tems, phen)

explainer_en <- DALEX::explain(model = meta_model, 
                               data = testdata[,-17],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")

model_parts_ensemble_pop <- model_parts(explainer_en, type = "variable_importance", variable_groups = grouped_vars,
                                        N = 5000, B = 100)
head(model_parts_ensemble_pop, 8)
plot(model_parts_ensemble_pop)

model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "salt_tidal_high_marsh_1000; salt_tidal_marsh_1000; deep_sand_grass_swale_marsh_1000; algal_flats_1000; dune_coast_grass_1000"] <- "TEMS (n = 5)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "baren_1000; forest_1000; est_emer_wet_8000; grass_8000; pal_aq_bed_4000; pal_aq_bed_8000; shrub_1000; water_1000"] <- "C-CAP (n = 8)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "d_data"] <- "Drought status (n =1)"
model_parts_ensemble_pop$variable[model_parts_ensemble_pop$variable == "tin_1000; maxn_1000"] <- "Phenology (n = 2)"
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
saveRDS(model_parts_ensemble_pop2, "pop_var_dat_group.RDS")
saveRDS(model_parts_ensemble_pop, "model_parts_ensemble_pop_group.RDS")

model_parts_ensemble_pop2 <- readRDS("pop_var_dat_group.RDS")
model_parts_ensemble_pop <- readRDS("model_parts_ensemble_pop_group.RDS")

ggplot_imp(model_parts_ensemble_pop2, model_parts_ensemble_pop)

########################################################
# HR ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("HR_level_meta_model.RDS")
preProcValues <- readRDS("preProcValues_HR.RDS")
data1_trn <- readRDS("data1_trn_HR.RDS")
testTransformed <- readRDS("testTransformed_HR.RDS")
trainTransformed <- readRDS("trainTransformed_HR.RDS")

drought <- list(c("d_data4"))
lidar <- list(c("veg_ht_8000"))
tems <- list(c( "deep_sand_live_oak_marsh_4000", "deep_sand_shrub_2000",
                "dune_coast_grass_4000",  "salt_tidal_marsh_4000",
                "salt_tidal_marsh_500"))
phen <- list(c("EOST_4000", "SOSN_4000",  "TIN_4000"))

grouped_vars <- c(drought, lidar, tems, phen)

y <- testTransformed$type
y <- as.character(y)
y[y == "random"] <- 1
y[y == "used"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

explainer_en <- DALEX::explain(model = meta_model$models$rf, 
                               data = testTransformed[,-11],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")


model_parts_ensemble_HR <- model_parts(explainer_en, variable_groups = grouped_vars, type = "variable_importance",
                                        N = 5000, B = 100)
head(model_parts_ensemble_HR, 8)
plot(model_parts_ensemble_HR)

model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "deep_sand_live_oak_marsh_4000; deep_sand_shrub_2000; dune_coast_grass_4000; salt_tidal_marsh_4000; salt_tidal_marsh_500"] <- "TEMS (n = 5)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "d_data4"] <- "Drought status (n =1)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "EOST_4000; SOSN_4000; TIN_4000"] <- "Phenology (n = 3)"
model_parts_ensemble_HR$variable[model_parts_ensemble_HR$variable == "veg_ht_8000"] <- "LiDAR (n = 1)"

plot(model_parts_ensemble_HR)
parm_keep <- c(unique(model_parts_ensemble_HR$variable))

model_parts_ensemble_HR1 <- as.data.frame(model_parts_ensemble_HR)
model_parts_ensemble_HR1$variable <- factor(model_parts_ensemble_HR1$variable)
model_parts_ensemble_HR2 <- subset(model_parts_ensemble_HR1, variable %in% parm_keep)

ggplot_imp(model_parts_ensemble_HR2, model_parts_ensemble_HR)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_HR2, "HR_var_dat_group.RDS")
saveRDS(model_parts_ensemble_HR, "model_parts_ensemble_HR_group.RDS")

model_parts_ensemble_HR2 <- readRDS("HR_var_dat_group.RDS")
model_parts_ensemble_HR <- readRDS("model_parts_ensemble_HR_group.RDS")

ggplot_imp(model_parts_ensemble_HR2, model_parts_ensemble_HR)

########################################################
# within HR - roost ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_meta_model_roost.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
preProcValues <- readRDS("preProcValues_within_HR_roosting.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_roosting.RDS")
testTransformed <- readRDS("testTransformed_within_HR_roosting.RDS")
data1_trn <- readRDS("trainuntrans_within_HR_roosting.RDS")

y <- testTransformed$type
y <- as.character(y)
y[y == "random"] <- 1
y[y == "used"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

lidar <- list(c("veg_ht_25", "chm_eucdist_3ha_clp"))
storm <- list(c("surge", "time_elapsed"))
ccap <- list(c("developed_1000",  "grass_1000", "water_1000", "pal_shrub_wet_500", "grass_125",
               "baren_50", "pal_emer_wet_50", "water_50"))
tems <- list(c("est_aq_bed_500", "ox_daisy_flats_500", "riparian_herb_veg_500",  
               "algal_flats_125", "salt_tidal_marsh_50", "riparian_herb_veg_125",
               "salt_tidal_high_marsh_50", "tidal_flats_125" ))
phen <- list(c("SOSN_1000", "TIN_1000", "DUR_500"))
topo <- list(c("tpi_data", "barth_dem_25"))
time <- list(c("doy" ))


grouped_vars <- c(lidar, storm, ccap, tems, phen, topo, time)

explainer_en <- DALEX::explain(model = meta_model$models$svmRadial, 
                               data = testTransformed[,-27],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")

model_parts_ensemble_roost <- model_parts(explainer_en, type = "variable_importance",
                                       N = 5000, variable_groups = grouped_vars, b = 100)
head(model_parts_ensemble_roost, 8)


model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "veg_ht_25; chm_eucdist_3ha_clp"] <- "LiDAR (n = 2)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "SOSN_1000; TIN_1000; DUR_500"] <- "Phenology (n = 3)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "surge; time_elapsed"] <- "Storm surge (n = 2)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "est_aq_bed_500; ox_daisy_flats_500; riparian_herb_veg_500; algal_flats_125; salt_tidal_marsh_50; riparian_herb_veg_125; salt_tidal_high_marsh_50; tidal_flats_125"] <- "TEMS (n = 8)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "developed_1000; grass_1000; water_1000; pal_shrub_wet_500; grass_125; baren_50; pal_emer_wet_50; water_50"] <- "C-CAP (n = 10)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "tpi_data; barth_dem_25"] <- "Topography (n = 2)"
model_parts_ensemble_roost$variable[model_parts_ensemble_roost$variable == "doy"] <- "Day of year (n = 1)"
plot(model_parts_ensemble_roost)
parm_keep <- c(unique(model_parts_ensemble_roost$variable))

model_parts_ensemble_roost1 <- as.data.frame(model_parts_ensemble_roost)
model_parts_ensemble_roost1$variable <- factor(model_parts_ensemble_roost1$variable)
model_parts_ensemble_roost2 <- subset(model_parts_ensemble_roost1, variable %in% parm_keep)

ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_roost2, "Roost_var_dat.RDS")
saveRDS(model_parts_ensemble_roost, "model_parts_ensemble_roost.RDS")
model_parts_ensemble_roost2 <- readRDS("Roost_var_dat.RDS")
model_parts_ensemble_roost <- readRDS("model_parts_ensemble_roost.RDS")
ggplot_imp(model_parts_ensemble_roost2, model_parts_ensemble_roost)

########################################################
# within HR - day ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_ensemble_model_fire_day.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
preProcValues <- readRDS("preProcValues_within_HR_fire_day.RDS")
testTransformed <- readRDS("testTransformed_within_HR_fire_day.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day.RDS")

y <- testTransformed$type
y <- as.character(y)
y[y == "random"] <- 1
y[y == "used"] <- 0
y <- as.numeric(as.character(y))
pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

lidar <- list(c("chm_eucdist_3ha_clp", "veg_ht_25"))
storm <- list(c("surge", "time_elapsed"))
ccap <- list(c("developed_500", "est_shrub_wet_1000", "grass_1000", "pal_shrub_wet_250.2","forest_500",                   
               "shrub_500", "water_1000", "water_50" , "pal_emer_wet_50"              ))
tems <- list(c("deep_sand_live_oak_250.2" ,  "deep_sand_live_oak_shrub_125",
               "riparian_herb_veg_125", "salt_tidal_marsh_50", "salt_tidal_high_marsh_50", "tidal_flats_125"))
phen <- list(c("DUR_500", "SOSN_1000", "TIN_1000"))
topo <- list(c("barth_dem_25", "tpi_data"))
fire <- list(c("recent", "freq"))
season <- list(c("doy"))

grouped_vars <- c(lidar, storm, ccap, tems, phen, topo, fire, season)

explainer_en <- DALEX::explain(model = meta_model$models$gbm, 
                               data = testTransformed[,-28],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")


model_parts_ensemble_day <- model_parts(explainer_en, type = "variable_importance",
                                       N = 5000, variable_groups = grouped_vars, B = 100)
head(model_parts_ensemble_day, 12)
plot(model_parts_ensemble_day)

model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "chm_eucdist_3ha_clp; veg_ht_25"] <- "LiDAR (n = 2)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "DUR_500; SOSN_1000; TIN_1000"] <- "Phenology (n = 3)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "recent; freq"] <- "Fire history (n = 2)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "surge; time_elapsed"] <- "Storm surge (n = 2)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "deep_sand_live_oak_250.2; deep_sand_live_oak_shrub_125; riparian_herb_veg_125; salt_tidal_marsh_50; salt_tidal_high_marsh_50; tidal_flats_125"] <- "TEMS (n = 6)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "developed_500; est_shrub_wet_1000; grass_1000; pal_shrub_wet_250.2; forest_500; shrub_500; water_1000; water_50; pal_emer_wet_50"] <- "C-CAP (n = 9)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "barth_dem_25; tpi_data"] <- "Topography (n = 2)"
model_parts_ensemble_day$variable[model_parts_ensemble_day$variable == "doy"] <- "Day of year (n = 1)"

parm_keep <- c(unique(model_parts_ensemble_day$variable))

model_parts_ensemble_day1 <- as.data.frame(model_parts_ensemble_day)
model_parts_ensemble_day1$variable <- factor(model_parts_ensemble_day1$variable)
model_parts_ensemble_day2 <- subset(model_parts_ensemble_day1, variable %in% parm_keep)

ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day)
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
saveRDS(model_parts_ensemble_day2, "Day_var_dat_group.RDS")
saveRDS(model_parts_ensemble_day, "model_parts_ensemble_day_group.RDS")
model_parts_ensemble_day2 <- readRDS("Day_var_dat_group.RDS")
model_parts_ensemble_day <- readRDS("model_parts_ensemble_day_group.RDS")
ggplot_imp(model_parts_ensemble_day2, model_parts_ensemble_day)
