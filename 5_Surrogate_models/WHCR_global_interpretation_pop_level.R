library(iml)
library(caret)
library(gbm)
library("ggplot2")
library(mefa)
library(randomForest)
library(rpart)
library(rpart.plot)

options(scipen = 999)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("pop_level_meta_model_5_30_23.RDS")
preProcValues <- readRDS("preProcValues_population_5_30_23.RDS")
testdata <- readRDS("testTransformed_population_5_30_23.RDS")
traindata <- readRDS("trainTransformed_population_5_30_23.RDS" )
data1_trn <- readRDS("untrans_population_5_30_23.RDS")
data.smote <- readRDS("smote_population_5_30_23.RDS")
traindata <- predict(preProcValues, data1_trn)
true_y_data <- data1_trn[,17]
data1_trn <- data1_trn[,-17]
### Below is to make names easier to understand and give numbers in %
names(data1_trn)
names(data1_trn)[14] <- "% salt tidal marsh (1km)"
data1_trn[,14] <- data1_trn[,14]*100
names(data1_trn)[11] <- "% deep sand grass swale marsh (1km)"
data1_trn[,11] <- data1_trn[,11]*100
names(data1_trn)[1] <- "Palmer drought serverity index (4km)"
data1_trn[,1] <- data1_trn[,1]/100
names(data1_trn)[12] <- "% dune coast grass (1km)"
data1_trn[,12] <- round(data1_trn[,12]*100, 3)
names(data1_trn)[9] <- "% palustrine aquatic bed (8km)"
data1_trn[,9] <- round(data1_trn[,9]*100, 3)
names(data1_trn)[7] <- "% estuarine emergent wetland (8km)"
data1_trn[,7] <- round(data1_trn[,7]*100, 3)
names(data1_trn)[5] <- "% open water (1km)"
data1_trn[,5] <- round(data1_trn[,5]*100, 3)
names(data1_trn)[6] <- "% palustrine aquatic bed (4km)"
data1_trn[,6] <- data1_trn[,6]*100
names(data1_trn)[8] <- "% grassland (8km)"
data1_trn[,8] <- data1_trn[,8]*100
names(data1_trn)[4] <- "% shrubland (1km)"
data1_trn[,4] <- data1_trn[,4]*100


model_predictions <- 1- (predict(meta_model$models$xgbTree, traindata, type = "prob")[,1])
plot(traindata$pop_data, model_predictions)

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
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, digits = 2)
rpart.rules(pruned_model)



# pruned model is not the same as global explainer
# global explainer has 0.59 r-square value 


y_hat <- predict(pruned_model, data1_trn)
# r squared
(cor(data1_trn$model_pre, y_hat))^2 
plot(data1_trn$model_pre, y_hat)

##################################
# Avoid Sci notation in figure ###
##################################

myformat0 <- function(x, digits=2)
{
  rpart.plot:::check.integer.scalar(digits)
  if(digits == 0)
    digits <- getOption("digits")
  if(digits >= 0)
    rpart.plot:::formate(x, digits, smallest=.0001, largest=100000)
  else
    sapply(x, format, digits=-digits)
}

assignInNamespace("format0", myformat0, ns = "rpart.plot")

#############################################################


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Figures/Figures_for_pub")
jpeg("population_explainer_model.jpeg", res = 300, quality = 100, width = 5500, height = 3400)
rpart.plot(pruned_model, type = 5, clip.right.labs = TRUE, branch = 0.4, under = TRUE, tweak = 1.4, digits = 2)
dev.off()

