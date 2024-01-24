library(caret)
library(gbm)
library(DALEX)
library(mltools)

########################################################
# Population ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("pop_level_meta_model.RDS")
preProcValues <- readRDS("preProcValues_population.RDS")
testdata <- readRDS("testTransformed_population.RDS")
trainTransformed <- readRDS("trainTransformed_population.RDS" )
data1_trn <- readRDS("untrans_population.RDS")

pred_ensemble <- predict(meta_model, testdata)
pred_ensemble_glm <- predict(meta_model$models$glm, testdata)
pred_ensemble_rf <- predict(meta_model$models$rf, testdata)
pred_ensemble_xgb <- predict(meta_model$models$xgbTree, testdata)
pred_ensemble_svm <- predict(meta_model$models$svmRadial, testdata)
pred_ensemble_gbm <- predict(meta_model$models$gbm, testdata)



pred <- function(model, newdata)  {
  results <- as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

y <- testdata$pop_data
y <- as.character(y)
y[y == "Neg"] <- 1
y[y == "Pos"] <- 0
y <- as.numeric(as.character(y))


explainer_en <- DALEX::explain(model = meta_model, 
                               data = testdata[,-17],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")
explainer_en_glm <- DALEX::explain(model = meta_model$models$glm, 
                                   data = testdata[,-17],
                                   y = y, 
                                   predict_function = pred,
                                   label = "ensemble",
                                   type = "classification")
explainer_en_rf <- DALEX::explain(model = meta_model$models$rf, 
                                  data = testdata[,-17],
                                  y = y, 
                                  predict_function = pred,
                                  label = "ensemble",
                                  type = "classification")
explainer_en_xgb <- DALEX::explain(model = meta_model$models$xgbTree, 
                                   data = testdata[,-17],
                                   y = y, 
                                   predict_function = pred,
                                   label = "ensemble",
                                   type = "classification")
explainer_en_svm <- DALEX::explain(model = meta_model$models$svmRadial, 
                                   data = testdata[,-17],
                                   y = y, 
                                   predict_function = pred,
                                   label = "ensemble",
                                   type = "classification")
explainer_en_gbm <- DALEX::explain(model = meta_model$models$gbm, 
                                   data = testdata[,-17],
                                   y = y, 
                                   predict_function = pred,
                                   label = "ensemble",
                                   type = "classification")



model_performance(explainer_en)
model_performance(explainer_en_glm)
model_performance(explainer_en_rf)
model_performance(explainer_en_xgb)
model_performance(explainer_en_gbm)
model_performance(explainer_en_svm)

mltools::mcc(pred_ensemble, testdata$pop_data)
mltools::mcc(pred_ensemble_glm, testdata$pop_data)
mltools::mcc(pred_ensemble_gbm, testdata$pop_data)
mltools::mcc(pred_ensemble_rf, testdata$pop_data)
mltools::mcc(pred_ensemble_svm, testdata$pop_data)
mltools::mcc(pred_ensemble_xgb, testdata$pop_data)

sedi_fun <- function(prediction){
  cmen <- confusionMatrix(prediction, testdata$pop_data)
  TPR.Table2 <- cmen$table[2,2]/sum(cmen$table[,2])
  FPR.Table2 <- cmen$table[2,1]/sum(cmen$table[,1])
  BiodiversityR::ensemble.SEDI(TPR=TPR.Table2, FPR=FPR.Table2)
}

c(sedi_fun(pred_ensemble),sedi_fun(pred_ensemble_glm),sedi_fun(pred_ensemble_rf),
  sedi_fun(pred_ensemble_svm), sedi_fun(pred_ensemble_xgb),sedi_fun(pred_ensemble_gbm))

########################################################
# HR ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("HR_level_meta_model.RDS")
preProcValues <- readRDS("preProcValues_HR.RDS")
data1_trn <- readRDS("data1_trn_HR.RDS")
testTransformed <- readRDS("testTransformed_HR.RDS")
trainTransformed <- readRDS("trainTransformed_HR.RDS")

pred_ensemble <- predict(meta_model, testTransformed)
pred_ensemble_glm <- predict(meta_model$models$glm, testTransformed)
pred_ensemble_rf <- predict(meta_model$models$rf, testTransformed)
pred_ensemble_xgb <- predict(meta_model$models$xgbTree, testTransformed)
pred_ensemble_svm <- predict(meta_model$models$svmRadial, testTransformed)
pred_ensemble_gbm <- predict(meta_model$models$gbm, testTransformed)


pred <- function(model, newdata)  {
  results <- 1 - as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

y <- testTransformed$type
y <- as.character(y)
y[y == "used"] <- 1
y[y == "random"] <- 0
y <- as.numeric(as.character(y))


explainer_en <- DALEX::explain(model = meta_model, 
                               data = testTransformed[,-50],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")
explainer_en_glm <- DALEX::explain(model = meta_model$models$glm, 
                                   data = testTransformed[,-50],
                                   y = y, 
                                   predict_function = pred,
                                   label = "glm",
                                   type = "classification")
explainer_en_rf <- DALEX::explain(model = meta_model$models$rf, 
                                  data = testTransformed[,-50],
                                  y = y, 
                                  predict_function = pred,
                                  label = "rf",
                                  type = "classification")
explainer_en_xgb <- DALEX::explain(model = meta_model$models$xgbTree, 
                                   data = testTransformed[,-50],
                                   y = y, 
                                   predict_function = pred,
                                   label = "xgb",
                                   type = "classification")
explainer_en_svm <- DALEX::explain(model = meta_model$models$svmRadial, 
                                   data = testTransformed[,-50],
                                   y = y, 
                                   predict_function = pred,
                                   label = "svm",
                                   type = "classification")
explainer_en_gbm <- DALEX::explain(model = meta_model$models$gbm, 
                                   data = testTransformed[,-50],
                                   y = y, 
                                   predict_function = pred,
                                   label = "gbm",
                                   type = "classification")



model_performance(explainer_en)
model_performance(explainer_en_glm)
model_performance(explainer_en_rf)
model_performance(explainer_en_svm)
model_performance(explainer_en_xgb)
model_performance(explainer_en_gbm)


c(mcc(pred_ensemble, testTransformed$type),mcc(pred_ensemble_glm, testTransformed$type),
  mcc(pred_ensemble_rf, testTransformed$type),mcc(pred_ensemble_svm, testTransformed$type),
  mcc(pred_ensemble_xgb, testTransformed$type),mcc(pred_ensemble_gbm, testTransformed$type))

sedi_fun <- function(prediction){
  cmen <- confusionMatrix(prediction, testTransformed[,11])
  TPR.Table2 <- cmen$table[2,2]/sum(cmen$table[,2])
  FPR.Table2 <- cmen$table[2,1]/sum(cmen$table[,1])
  BiodiversityR::ensemble.SEDI(TPR=TPR.Table2, FPR=FPR.Table2)
}

c(sedi_fun(pred_ensemble),sedi_fun(pred_ensemble_glm),sedi_fun(pred_ensemble_rf),
  sedi_fun(pred_ensemble_svm), sedi_fun(pred_ensemble_xgb),sedi_fun(pred_ensemble_gbm))

########################################################
# daytime ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_ensemble_model_fire_day.RDS")
preProcValues <- readRDS("preProcValues_within_HR_fire_day.RDS")
testTransformed <- readRDS("testTransformed_within_HR_fire_day.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day.RDS")

pred_ensemble <- predict(meta_model, testTransformed)
pred_ensemble_glm <- predict(meta_model$models$glm, testTransformed)
pred_ensemble_rf <- predict(meta_model$models$rf, testTransformed)
pred_ensemble_xgb <- predict(meta_model$models$xgbTree, testTransformed)
pred_ensemble_svm <- predict(meta_model$models$svmRadial, testTransformed)
pred_ensemble_gbm <- predict(meta_model$models$gbm, testTransformed)


pred <- function(model, newdata)  {
  results <- 1 - as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

y <- testTransformed$type
y <- as.character(y)
y[y == "used"] <- 1
y[y == "random"] <- 0
y <- as.numeric(as.character(y))


explainer_en <- DALEX::explain(model = meta_model, 
                               data = testTransformed[,-28],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")
explainer_en_glm <- DALEX::explain(model = meta_model$models$glm, 
                                   data = testTransformed[,-28],
                                   y = y, 
                                   predict_function = pred,
                                   label = "glm",
                                   type = "classification")
explainer_en_rf <- DALEX::explain(model = meta_model$models$rf, 
                                  data = testTransformed[,-28],
                                  y = y, 
                                  predict_function = pred,
                                  label = "rf",
                                  type = "classification")
explainer_en_xgb <- DALEX::explain(model = meta_model$models$xgbTree, 
                                   data = testTransformed[,-28],
                                   y = y, 
                                   predict_function = pred,
                                   label = "xgb",
                                   type = "classification")
explainer_en_svm <- DALEX::explain(model = meta_model$models$svmRadial, 
                                   data = testTransformed[,-28],
                                   y = y, 
                                   predict_function = pred,
                                   label = "svm",
                                   type = "classification")
explainer_en_gbm <- DALEX::explain(model = meta_model$models$gbm, 
                                   data = testTransformed[,-28],
                                   y = y, 
                                   predict_function = pred,
                                   label = "gbm",
                                   type = "classification")



model_performance(explainer_en)
model_performance(explainer_en_glm)
model_performance(explainer_en_rf)
model_performance(explainer_en_svm)
model_performance(explainer_en_xgb)
model_performance(explainer_en_gbm)


c(mcc(pred_ensemble, testTransformed$type),mcc(pred_ensemble_glm, testTransformed$type),
  mcc(pred_ensemble_rf, testTransformed$type),mcc(pred_ensemble_svm, testTransformed$type),
  mcc(pred_ensemble_xgb, testTransformed$type),mcc(pred_ensemble_gbm, testTransformed$type))

sedi_fun <- function(prediction){
  cmen <- confusionMatrix(prediction, testTransformed$type)
  TPR.Table2 <- cmen$table[2,2]/sum(cmen$table[,2])
  FPR.Table2 <- cmen$table[2,1]/sum(cmen$table[,1])
  BiodiversityR::ensemble.SEDI(TPR=TPR.Table2, FPR=FPR.Table2)
}

c(sedi_fun(pred_ensemble),sedi_fun(pred_ensemble_glm),sedi_fun(pred_ensemble_rf),
  sedi_fun(pred_ensemble_svm), sedi_fun(pred_ensemble_xgb),sedi_fun(pred_ensemble_gbm))

########################################################
# roosting ###########################################
########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
meta_model <- readRDS("within_HR_level_meta_model_roost.RDS")
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
preProcValues <- readRDS("preProcValues_within_HR_roosting.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_roosting.RDS")
testTransformed <- readRDS("testTransformed_within_HR_roosting.RDS")

pred_ensemble <- predict(meta_model, testTransformed)
pred_ensemble_glm <- predict(meta_model$models$glm, testTransformed)
pred_ensemble_rf <- predict(meta_model$models$rf, testTransformed)
pred_ensemble_xgb <- predict(meta_model$models$xgbTree, testTransformed)
pred_ensemble_svm <- predict(meta_model$models$svmRadial, testTransformed)
pred_ensemble_gbm <- predict(meta_model$models$gbm, testTransformed)


pred <- function(model, newdata)  {
  results <- 1 - as.data.frame(predict(model, newdata, type = "prob"))
  return(results[,1])
}

y <- testTransformed$type
y <- as.character(y)
y[y == "used"] <- 1
y[y == "random"] <- 0
y <- as.numeric(as.character(y))


explainer_en <- DALEX::explain(model = meta_model, 
                               data = testTransformed[,-29],
                               y = y, 
                               predict_function = pred,
                               label = "ensemble",
                               type = "classification")
explainer_en_glm <- DALEX::explain(model = meta_model$models$glm, 
                                   data = testTransformed[,-29],
                                   y = y, 
                                   predict_function = pred,
                                   label = "glm",
                                   type = "classification")
explainer_en_rf <- DALEX::explain(model = meta_model$models$rf, 
                                  data = testTransformed[,-29],
                                  y = y, 
                                  predict_function = pred,
                                  label = "rf",
                                  type = "classification")
explainer_en_xgb <- DALEX::explain(model = meta_model$models$xgbTree, 
                                   data = testTransformed[,-29],
                                   y = y, 
                                   predict_function = pred,
                                   label = "xgb",
                                   type = "classification")
explainer_en_svm <- DALEX::explain(model = meta_model$models$svmRadial, 
                                   data = testTransformed[,-29],
                                   y = y, 
                                   predict_function = pred,
                                   label = "svm",
                                   type = "classification")
explainer_en_gbm <- DALEX::explain(model = meta_model$models$gbm, 
                                   data = testTransformed[,-29],
                                   y = y, 
                                   predict_function = pred,
                                   label = "gbm",
                                   type = "classification")



model_performance(explainer_en)
model_performance(explainer_en_glm)
model_performance(explainer_en_rf)
model_performance(explainer_en_svm)
model_performance(explainer_en_xgb)
model_performance(explainer_en_gbm)


c(mcc(pred_ensemble, testTransformed$type),mcc(pred_ensemble_glm, testTransformed$type),
  mcc(pred_ensemble_rf, testTransformed$type),mcc(pred_ensemble_svm, testTransformed$type),
  mcc(pred_ensemble_xgb, testTransformed$type),mcc(pred_ensemble_gbm, testTransformed$type))

sedi_fun <- function(prediction){
  cmen <- confusionMatrix(prediction, testTransformed$type)
  TPR.Table2 <- cmen$table[2,2]/sum(cmen$table[,2])
  FPR.Table2 <- cmen$table[2,1]/sum(cmen$table[,1])
  BiodiversityR::ensemble.SEDI(TPR=TPR.Table2, FPR=FPR.Table2)
}

c(sedi_fun(pred_ensemble),sedi_fun(pred_ensemble_glm),sedi_fun(pred_ensemble_rf),
  sedi_fun(pred_ensemble_svm), sedi_fun(pred_ensemble_xgb),sedi_fun(pred_ensemble_gbm))

