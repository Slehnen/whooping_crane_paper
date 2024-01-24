library(caret)
library(caretEnsemble)
library(gbm)
library(stringr)
library(fastDummies)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
all_data1 <- readRDS("S2_level_data.RDS")

all_data1 <- na.omit(all_data1)
dim(all_data1)
all_data1$type <- as.factor(all_data1$type)
summary(all_data1$type)
summary(all_data1$id)


all_data1$age <- factor(str_sub(all_data1$id, start= -3)) 

all_data1 <- dummy_cols(all_data1, 
                        select_columns = "age")
head(all_data1)
all_data1$age <- NULL

dim(all_data1)
summary(all_data1)
summary(all_data1$type)

######################################################
#### Remove variables with low information ##########
######################################################

index <- nearZeroVar(all_data1, freqCut = 30,uniqueCut = 1)
length(index)
names(all_data1)[index]
head(all_data1[,index])
all_data1 <- all_data1[,-index]
names(all_data1)
dim(all_data1)

######################################################
#### Remove variables with high correlations ##########
######################################################
take_out <- c("id", "type", "age_Adt", "age_Juv", "age_Sub")
scale_data <- all_data1[,-which(names(all_data1) %in% take_out)]

# calculate correlation matrix
correlationMatrix <- cor(scale_data)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

names(scale_data[,-highlyCorrelated])
names(scale_data[,highlyCorrelated])

scale_data <- scale_data[,-highlyCorrelated]

scale_data$id <- all_data1$id
scale_data$age_Adt<- all_data1$age_Adt
scale_data$age_Juv <- all_data1$age_Juv
scale_data$age_Sub <- all_data1$age_Sub
scale_data$type <- all_data1$type
all_data1 <- scale_data

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
birds_train <- read.csv("bird_IDS_training.csv")[,2]
birds_test <- read.csv("bird_IDS_testing.csv")[,2]

default_idx <- which(substr(all_data1$id, 1, 3) %in% birds_train)
data1_trn <- all_data1[default_idx, ]
data1_trn$id <- NULL
data1_tst <- all_data1[-default_idx, ]
data1_tst$id <- NULL


###############################################################
######### remove unimportant variables  ########
###############################################################

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

x <- data1_trn[,1:49]

# Target variable
y <- data1_trn$type

# Training: 80%; Test: 20%
set.seed(2023)
inTrain <- createDataPartition(y, p = 0.80, list = FALSE)[,1]

x_train <- x[ inTrain, ]
x_test  <- x[-inTrain, ]

y_train <- y[ inTrain]
y_test  <- y[-inTrain]

index <- sample(1:length(y_train), size = 2000)
x_train_mini <- x_train[index,]
y_train_mini <- y_train[index]
# Run RFE
result_rfe1 <- rfe(x = x_train_mini, 
                   y = y_train_mini, 
                   sizes = c(6:14), # limited size based on results of earlier run
                   rfeControl = control)

# Print the results
result_rfe1

plot(result_rfe1, type = c("g", "o"))
# Print the selected features
predictors(result_rfe1)

data1_trn <- data1_trn[ , (names(data1_trn) %in% c(predictors(result_rfe1), "type"))]
data1_tst <- data1_tst[ , (names(data1_tst) %in% c(predictors(result_rfe1), "type"))]

############################################################
# Scale and center data ####################################
############################################################

preProcValues <- preProcess(data1_trn, method = c("center", "scale"))
testTransformed <- predict(preProcValues, data1_tst)
trainTransformed <- predict(preProcValues, data1_trn)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
saveRDS(preProcValues, "preProcValues_HR.RDS")
saveRDS(trainTransformed, "trainTransformed_HR.RDS")
saveRDS(testTransformed, "testTransformed_HR.RDS")
saveRDS(data1_tst, "data1_tst_HR.RDS")
saveRDS(data1_trn, "data1_trn_HR.RDS")

my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(trainTransformed$type),
  summaryFunction=twoClassSummary,
  verboseIter = TRUE,
)

tunegrid_rf <- expand.grid(.mtry=c(1:5))

grid_xgbTree <- expand.grid(
  nrounds = 500,
  max_depth = c(2, 3, 4, 5, 6),
  eta = c(0.01, 0.025, 0.05, 0.1, 0.3),
  gamma = c(0, 0.1, 0.3, 0.5, 0.7, 1.0),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)
modelTypes <- list(glm       = caretModelSpec(method="glm"),                                                                              # no tuning parameters possible for glms                                                                                           
                   rf        = caretModelSpec(method="rf",  classwt = c(0.5, 0.5), ntree = 1000, maxnodes = 2^4, tuneGrid = tunegrid_rf), #setting maxnodes to 2^5 restricts the maximum depth to 5, added to prevent overfitting
                   xgbTree   = caretModelSpec(method="xgbTree", tuneGrid = grid_xgbTree),
                   svmRadial = caretModelSpec(method="svmRadial"),                                                                          # for svmRadial, procedure automatically chooses the optimal values for the model tuning parameters                
                   gbm       = caretModelSpec(method="gbm", verbose=FALSE, tuneGrid=expand.grid(n.trees = c(700, 800, 900, 1000), interaction.depth = c(2, 3, 4, 5), shrinkage =0.1, n.minobsinnode = 10 )) 
)


model_list <- caretList(
  type~., data=trainTransformed,
  trControl=my_control,
  metric = "ROC",
  tuneList = modelTypes
)

xyplot(resamples(model_list))

ctrl <- trainControl(method="repeatedcv", number=5, repeats=3, returnResamp="final", savePredictions="final", classProbs=TRUE, selectionFunction="oneSE", verboseIter=TRUE)

models_stack <- caretStack(
  model_list,
  data=trainTransformed,
  tuneLength=10,
  method="glmnet",
  metric="ROC",
  trControl=ctrl
)

models_stack

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
saveRDS(models_stack, "HR_level_meta_model.RDS")
saveRDS(model_list, "HR_level_model_list.RDS")
meta_model <- readRDS("HR_level_meta_model.RDS")
trainTransformed <- readRDS("trainTransformed_HR.RDS")              

model_preds <- lapply(model_list, predict, newdata=testTransformed, type="prob")
model_preds <- data.frame(model_preds)
ens_preds <- predict(meta_model, newdata=testTransformed, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testTransformed$type)