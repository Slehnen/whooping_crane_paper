library(rpart)
library(rpart.plot)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_ensemble_model_fire_day.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
preProcValues <- readRDS("preProcValues_within_HR_fire_day.RDS")
testTransformed <- readRDS("testTransformed_within_HR_fire_day.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day.RDS")

data1_trn <- readRDS("trainuntrans_within_HR_fire_day.RDS")

names(data1_trn)[15] <- "% salt tidal marsh (50m)"
data1_trn[,15] <- data1_trn[,15]*100
names(data1_trn)[9] <- "% water (50m)"
data1_trn[,9] <- data1_trn[,9]*100
names(data1_trn)[19] <- "TPI" 
names(data1_trn)[24] <- "Elevation (25m)" 
names(data1_trn)[21] <- "Vegetation height (25m)" 
names(data1_trn)[20] <- "Distance from nearest shrub/tree clump (m)" 

true_y_data <- trainTransformed[,28]
trainTransformed <- trainTransformed[,-28]



model_predictions <-   1 - predict(meta_model, trainTransformed, type = "prob")
plot(true_y_data, model_predictions)
data1_trn$model_pre <- model_predictions

data1_trn <- data1_trn[,-28]
global_explainer <- rpart(model_pre~. ,  data=data1_trn, method="anova")
global_explainer

# get index of CP with lowest xerror
opt <- which.min(global_explainer$cptable[,"xerror"])
#get its value
cp <- global_explainer$cptable[opt, "CP"]
#prune tree
#prune tree
pruned_model <- prune(global_explainer,cp)
#plot tree
rpart.plot(global_explainer)
rpart.plot(pruned_model)

# pruned model is not the same as global explainer
# global explainer has 0.73 r-square value vs. 0.70 for pruned model 


y_hat <- predict(global_explainer, data1_trn)
# r squared
(cor(data1_trn$model_pre, y_hat))^2 
plot(data1_trn$model_pre, y_hat)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pub")
jpeg("daytime_explainer_model.jpeg", res = 300, quality = 100, width = 3800, height = 3000)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, tweak = 1.4, digits = 2)
dev.off()
