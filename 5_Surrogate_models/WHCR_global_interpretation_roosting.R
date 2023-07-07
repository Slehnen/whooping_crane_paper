library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(rpart)
library(rpart.plot)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_meta_model_roost_5_18_23.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
preProcValues <- readRDS("preProcValues_within_HR_roosting_5_18_23.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_roosting_5_18_23.RDS")
testTransformed <- readRDS("testTransformed_within_HR_roosting_5_18_23.RDS")
data1_trn <- readRDS("trainuntrans_within_HR_roosting_5_18_23.RDS")

true_y_data <- data1_trn[,27]
data1_trn <- data1_trn[,-27]
data_trn <- data1_trn

### Below is to make names easier to understand and give numbers in %
names(data1_trn)
names(data1_trn)[7] <- "% barren land (50m)"
data1_trn[,7] <- data1_trn[,7]*100
names(data1_trn)[22] <- "Vegetation height (25m)" 
names(data1_trn)[20] <- "TPI" 
names(data1_trn)[25] <- "Elevation (25m)" 
names(data1_trn)[8] <- "% palustrine emergent wetland (50m)"
data1_trn[,8] <- data1_trn[,8]*100


model_predictions <- 1 - predict(meta_model, trainTransformed, type = "prob")
plot(true_y_data, model_predictions)
data1_trn$model_pre <- model_predictions

global_explainer <- rpart(model_pre~. , data=data1_trn) 
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

# pruned model is not the same as global explainer
# global explainer has 0.73 r-square value vs. 0.70 for pruned model 


y_hat <- predict(global_explainer, data1_trn)
# r squared
(cor(data1_trn$model_pre, y_hat))^2 
plot(data1_trn$model_pre, y_hat)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pub")
jpeg("roosting_explainer_model.jpeg", res = 300, quality = 100, width = 3500, height = 2000)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, tweak = 1.4, digits = 2)
dev.off()

