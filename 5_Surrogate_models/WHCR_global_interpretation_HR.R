library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(rpart)
library(rpart.plot)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("HR_level_meta_model_5_18_2023.RDS")
preProcValues <- readRDS("preProcValues_HR_5_18_23.RDS")
data1_trn <- readRDS("data1_trn_HR_5_18_23.RDS")
testTransformed <- readRDS("testTransformed_HR_5_18_23.RDS")
trainTransformed <- readRDS("trainTransformed_HR_5_18_23.RDS")

true_y_data <- data1_trn[,11]
data1_trn <- data1_trn[,-11]
data_for_model <- data1_trn

names(data1_trn)[2] <- "Vegetation height (8km)"
names(data1_trn)[3] <- "% deep sand shrub (2km)"
data1_trn[,3] <- data1_trn[,3]*100
names(data1_trn)[4] <- "% deep sand live oak marsh (4km)"
data1_trn[,4] <- data1_trn[,4]*100
names(data1_trn)[5] <- "% dune coast grass (4km)"
data1_trn[,5] <- data1_trn[,5]*100
names(data1_trn)[1] <- "Palmer Drought Severity Index (4km)"
data1_trn[,1] <- data1_trn[,1]/10
names(data1_trn)[6] <- "% salt tidal marsh (4km)"
data1_trn[,6] <- data1_trn[,6]*100
names(data1_trn)[7] <- "% salt tidal marsh (500m)"
data1_trn[,7] <- data1_trn[,7]*100
names(data1_trn)[9] <- "NDVI at start of season (4km)"
data1_trn[,9] <- data1_trn[,9]/1000
names(data1_trn)[8] <- "End of season day (4km)"

model_predictions <- 1 - predict(meta_model, predict(preProcValues, data_for_model), type = "prob")
plot(true_y_data, model_predictions)
data1_trn$model_pre <- model_predictions

global_explainer <- rpart(model_pre~. ,  data=data1_trn, method="anova")
global_explainer

# get index of CP with lowest xerror
opt <- which.min(global_explainer$cptable[,"xerror"])
#get its value
cp <- global_explainer$cptable[opt, "CP"]
#prune tree
pruned_model <- prune(global_explainer,cp)
#plot tree
rpart.plot(global_explainer)
rpart.plot(pruned_model)

# pruned model is the same as global explainer

y_hat <- predict(pruned_model, data1_trn)
# r squared
(cor(data1_trn$model_pre, y_hat))^2 
plot(data1_trn$model_pre, y_hat)

rpart.plot(pruned_model,cex=3)
rpart.plot(pruned_model, cex = 0.4)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pub")
jpeg("HR_explainer_model.jpeg", res = 300, quality = 100, width = 5200, height = 2500)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, tweak = 1.1, digits = 2)
dev.off()

