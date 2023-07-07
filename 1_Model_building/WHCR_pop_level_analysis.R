library(raster)
library(sp)
library(boot)
library(arm)
library(glmnet)
library(pscl)
library(gbm)
library(rpart)
library(caret)
library(caretEnsemble)
library(pROC)
library(ROCR)
library(foreach)
library(doParallel)
library(performanceEstimation)

######################################################
##################### Random data generation #########
######################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
all_data1 <- readRDS("S1_level_data.RDS")

all_data1 <- all_data1[ , colSums(is.na(all_data1)) < 10000]
all_data1 <- na.omit(all_data1)
dim(all_data1)
all_data1$pop_data <- as.factor(all_data1$pop_data)
summary(all_data1$pop_data)

all_data1$amp_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "AMP")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$amp_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "AMP")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$amp_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "AMP")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$amp_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "AMP")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$dur_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "DUR")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$dur_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "DUR")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$dur_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "DUR")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$dur_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "DUR")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])


all_data1$sosn_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOSN")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$sosn_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOSN")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$sosn_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOSN")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$sosn_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOSN")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$sost_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOST")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$sost_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOST")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$sost_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOST")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$sost_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "SOST")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$eosn_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOSN")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$eosn_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOSN")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$eosn_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOSN")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$eosn_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOSN")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$eost_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOST")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$eost_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOST")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$eost_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOST")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$eost_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "EOST")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$tin_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "TIN")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$tin_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "TIN")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$tin_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "TIN")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$tin_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "TIN")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$maxt_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXT")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$maxt_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXT")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$maxt_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXT")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$maxt_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXT")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$maxn_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXN")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$maxn_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXN")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$maxn_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXN")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$maxn_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "MAXN")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$corn_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "corn")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$corn_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "corn")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$corn_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "corn")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$corn_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "corn")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

all_data1$sorghum_1000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "sorghum")==1 & sapply(names(all_data1), grep, pattern = "1000")==1)])
all_data1$sorghum_2000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "sorghum")==1 & sapply(names(all_data1), grep, pattern = "2000")==1)])
all_data1$sorghum_4000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "sorghum")==1 & sapply(names(all_data1), grep, pattern = "4000")==1)])
all_data1$sorghum_8000 <- rowMeans(all_data1[, which(sapply(names(all_data1), grep, pattern = "sorghum")==1 & sapply(names(all_data1), grep, pattern = "8000")==1)])

################################
### Train model ################
#################################

all_data1$deep_sand_grass_combo_8000 <- all_data1$deep_sand_grass_swale_marsh_8000 + all_data1$dune_coast_grass_8000
all_data1$deep_sand_grass_combo_4000 <- all_data1$deep_sand_grass_swale_marsh_4000 + all_data1$dune_coast_grass_4000
all_data1$deep_sand_woody_combo_8000 <- all_data1$deep_sand_shrub_8000 + all_data1$deep_sand_live_oak_8000
all_data1$pop_data <- as.numeric(as.character(all_data1$pop_data))


df.min <- t(sapply(all_data1, range))[,1]
df.max <- t(sapply(all_data1, range))[,2]
df.sd <- t(sapply(all_data1, sd))
df.mean <- t(sapply(all_data1, mean))

drops <- c("pop_data")
scale_data <- all_data1[ , !(names(all_data1) %in% drops)]

# Remove year specific variables 
rm <- c(6:61, 185:511)
scale_data  <- scale_data[, -rm]

# remove variables with very low variance
index <- nearZeroVar(scale_data, freqCut = 30,uniqueCut = 1)
length(index)
names(scale_data)[index]
head(scale_data[,index])
scale_data <- scale_data[,-index]

######################################################
#### Remove variables with high correlations ##########
######################################################

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

#####################################################
######### remove variables that inflate VIF ########
###################################################

scale_data$pop_data <- all_data1$pop_data

simple_glm <- glm(pop_data ~ ., data = scale_data)
car::vif(simple_glm)
which(car::vif(simple_glm)>10)
scale_data <- scale_data[,-which(car::vif(simple_glm)>10)]

drops <- c("TIN2018_8000")
scale_data <- scale_data[ , !(names(scale_data) %in% drops)]

###############################################################
######### remove unimportant variables  ########
###############################################################

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

x <- scale_data[,1:42]

# Target variable
y <- scale_data$pop_data

# Training: 80%; Test: 20%
set.seed(1234)
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
                   sizes = c(15:24), # limited size based on results of earlier run
                   rfeControl = control)

# Print the results
result_rfe1

# Print the selected features
predictors(result_rfe1)

scale_data <- scale_data[ , (names(scale_data) %in% c(predictors(result_rfe1), "pop_data"))]

##########################################################
### Create factor for analysis in caret ##################
##########################################################

scale_data$pop_data <- ifelse(scale_data$pop_data == 1, "Pos", "Neg")
scale_data$pop_data <- factor(scale_data$pop_data)

scale_data <- na.omit(scale_data)


###########################################################
# Partition into train and test sets #####################
###########################################################

default_idx <- createDataPartition(scale_data$pop_data, p = 0.7, list = FALSE)
data1_trn <- scale_data[default_idx, ]
data1_tst <- scale_data[-default_idx, ]


preProcValues <- preProcess(data1_trn, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, data1_trn)
testTransformed <- predict(preProcValues, data1_tst)

table(trainTransformed$pop_data)

data.smote <- smote(pop_data ~ ., trainTransformed , perc.over = 10, perc.under=1.1)
table(data.smote$pop_data)

boxplot(data.smote[,-(which(names(data.smote) %in% c("pop_data")))], col = "orange", main = "Features Boxplot")

rownames(data.smote) <- NULL
setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
saveRDS(preProcValues, "preProcValues_population_5_30_23.RDS")
saveRDS(trainTransformed, "trainTransformed_population_5_30_23.RDS")
saveRDS(testTransformed, "testTransformed_population_5_30_23.RDS")
saveRDS(data1_trn, "untrans_population_5_30_23.RDS")
saveRDS(data.smote, "smote_population_5_30_23.RDS")
trainTransformed <- readRDS("trainTransformed_population_5_30_23.RDS")
data.smote <- readRDS("smote_population_5_30_23.RDS")
testTransformed <- readRDS("testTransformed_population_5_30_23.RDS")

my_control <- trainControl(
  method="boot",
  number=25,
  savePredictions="final",
  classProbs=TRUE,
  index=createResample(data.smote$pop_data),
  summaryFunction=twoClassSummary,
  verboseIter = TRUE
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
  pop_data~., data=data.smote,
  trControl=my_control,
  metric = "ROC",
  tuneList = modelTypes
)

xyplot(resamples(model_list))

stackControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, savePredictions = TRUE, classProbs = TRUE, verboseIter = TRUE)

meta_model <- caretStack(
  model_list,
  method="rf",
  metric="ROC",
  trControl=stackControl
)

saveRDS(meta_model, "pop_level_meta_model_5_30_23.RDS")
saveRDS(model_list, "pop_model_list_5_18_23.RDS")
# Predict
model_preds <- lapply(meta_model$models, predict, newdata=testTransformed, type="prob")
model_preds <- do.call(cbind, model_preds)
model_preds <- data.frame(model_preds)
ens_preds <- predict(meta_model, newdata=testTransformed, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testTransformed$pop_data)
