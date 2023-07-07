library(raster)
library(sf)
library(sp)
library(boot)
library(arm)
library(glmnet)
library(pscl)
library(stringr)
library(pROC)
library(caret)
library(caretEnsemble)
library(gbm)
library(spatialEco)
library(lubridate)
library(suncalc)
library(stringr)
library(fastDummies)
library(spocc)
library(ROCR)
library(caTools)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
all_data1 <- readRDS("S3_level_data.RDS")

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Scripts/Pub_scripts/0_Data")
all_pts <- st_read("Within_HR_level_data.shp")

P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
all_pts2 <- st_transform(all_pts, P4S.latlon)

sundata <- as.data.frame(all_pts2)[,1:3]
sundata$date <- as.Date(sundata$date, format = "%Y-%m-%d %H:%M:%S", tz = "US/Central")
sundata$lon <- st_coordinates(all_pts2)[,1]
sundata$lat <- st_coordinates(all_pts2)[,2]
names(sundata) <- c( "date", "ID" , "type","lon", "lat")
suns <- getSunlightTimes(data = sundata,
                         keep = c( "dawn", "dusk"), tz = "US/Central")

all_data1 <- cbind(suns, all_data1)
all_data1 <- as.data.frame(all_data1)
date_index <- which(names(all_data1)=="date")

all_data1$date.1 <- strftime(all_data1[,date_index[2]], format = "%Y-%m-%d %H:%M:%S", tz = "US/Central")

all_data1$tod <- ifelse(all_data1$date.1 > all_data1$dawn & all_data1$date.1 < all_data1$dusk, "day", "roost")

all_data1$tod <- factor(all_data1$tod)
summary(all_data1$tod)
all_data1 <- subset(all_data1, tod == "day")

all_data1$doy <- yday(as.POSIXct(all_data1$date, format = '%Y-%m-%d %H:%M:%S'))
all_data1$doy[all_data1$doy<200] <- all_data1$doy[all_data1$doy<200]+366

all_data1$tod <- NULL
all_data1$date <- NULL
all_data1$date.1 <- NULL
all_data1$date.2 <- NULL
all_data1$lat <- NULL
all_data1$lon <- NULL
all_data1$dawn <- NULL
all_data1$dusk <- NULL

######################################################
# Remove variables with many missing values and/or very low variance ############
## Except for fire variables - use median values for those (or max for areas with high water unlikely to burn)
#####################################################

all_data1$freq[is.na(all_data1$freq) & all_data1$water_50>0.50] <- max(all_data1$freq, na.rm=TRUE)
all_data1$freq[is.na(all_data1$freq)] <- median(all_data1$freq, na.rm=TRUE)
all_data1$recent[is.na(all_data1$recent)& all_data1$water_50>0.50] <- max(all_data1$recent, na.rm=TRUE)
all_data1$recent[is.na(all_data1$recent)] <- median(all_data1$recent, na.rm=TRUE)
all_data1$burn_prev[is.na(all_data1$burn_prev)& all_data1$water_50>0.50] <- max(all_data1$burn_prev, na.rm=TRUE)
all_data1$burn_prev[is.na(all_data1$burn_prev)] <- median(all_data1$burn_prev, na.rm=TRUE)

index_na <- colSums(is.na(all_data1))
index_na <- which(index_na>10000)
all_data1 <- all_data1[,-index_na]

index <- nearZeroVar(all_data1, freqCut = 30,uniqueCut = 1)
length(index)
names(all_data1)[index]
head(all_data1[,index])
all_data1 <- all_data1[,-index]
names(all_data1)
dim(all_data1)

summary(all_data1$type)

######################################################
# Remove variables with high correlations ############
#####################################################

scale_data <- all_data1
scale_data <- na.omit(scale_data)
drop <- c(names(which(unlist(lapply(scale_data, is.numeric))==FALSE)))
scale_data <- scale_data[-which(names(scale_data) %in% drop)]
# calculate correlation matrix
correlationMatrix <- cor(scale_data)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE)
# print indexes of highly correlated attributes
print(highlyCorrelated)
highlyCorrelated <- highlyCorrelated[!highlyCorrelated %in% "recent"] 

names(scale_data[,!(names(scale_data) %in% highlyCorrelated)])

all_data1 <- all_data1[,!(names(all_data1) %in% highlyCorrelated)]
dim(all_data1)
summary(all_data1$type)
names(all_data1)

all_data1 <- na.omit(all_data1)
dim(all_data1)
all_data1$type <- as.factor(all_data1$type)
summary(all_data1$type)
summary(all_data1$id)


out <- c("type", "id")
all_data2 <- all_data1[,! names(all_data1) %in% out]

df.sd <- t(sapply(all_data2, sd))
df.mean <- t(sapply(all_data2, mean))

all_data2$type <- all_data1$type
all_data2$id <- all_data1$id

all_data2$age <- factor(str_sub(all_data2$id, start= -3)) 

# Create dummy variable
all_data2 <- dummy_cols(all_data2, 
                        select_columns = "age")
head(all_data2)

############################################################
#### Separate into training and testing sets by id #######
###########################################################

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work")
birds_train <- read.csv("bird_IDS_training.csv")[,2]
birds_test <- read.csv("bird_IDS_testing.csv")[,2]

all_data2$birds <- substr(all_data2$id, 1, 3)
data1_trn <- subset(all_data2, birds %in% birds_train)
data1_trn$id <- NULL
data1_trn$age <- NULL
data1_trn$birds <- NULL
data1_tst <- subset(all_data2, birds %in% birds_test)
data1_tst$id <- NULL
data1_tst$age <- NULL
data1_tst$birds <- NULL

dim(data1_trn)
summary(data1_trn)
summary(data1_trn$type)

###############################################################
######### remove unimportant variables  ########
###############################################################

control <- rfeControl(functions = rfFuncs, # random forest
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 10) # number of folds

x <- data1_trn[,c(1:44, 46:48)]

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
                   sizes = c(21:29), # limited size based on results of earlier run
                   rfeControl = control)

# Print the results
result_rfe1

plot(result_rfe1, type = c("g", "o")) 

# Print the selected features
predictors(result_rfe1)

index <- which(names(data1_trn) %in% c(predictors(result_rfe1), "type"))
data1_trn <- data1_trn[,index]
data1_tst <- data1_tst[,index]

###################################################################
##### Center and scale data ######################################
#################################################################

preProcValues <- preProcess(data1_trn, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, data1_trn)
testTransformed <- predict(preProcValues, data1_tst)
dim(trainTransformed)

setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
saveRDS(preProcValues, "preProcValues_within_HR_fire_day_5_21_23.RDS")
saveRDS(trainTransformed, "trainTransformed_within_HR_fire_day_5_21_23.RDS")
saveRDS(testTransformed, "testTransformed_within_HR_fire_day_5_21_23.RDS")
saveRDS(data1_trn, "trainuntrans_within_HR_fire_day_5_21_23.RDS")
trainTransformed <- readRDS("trainTransformed_within_HR_fire_day_5_21_23.RDS")

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

meta_model <- caretStack(
  model_list,
  data=trainTransformed,
  tuneLength=10,
  method="glmnet",
  metric="ROC",
  trControl=ctrl
)


summary(meta_model)


setwd("C:/Users/slehnen/OneDrive - DOI/WHCR/Work/final_models")
saveRDS(model_list, "within_HR_level_model_list_day_5_18_23.RDS")
saveRDS(meta_model, "within_HR_level_ensemble_model_fire_day_5_21_23.RDS")

model_preds <- lapply(meta_model$models, predict, newdata=testTransformed, type="prob")
model_preds <- data.frame(model_preds)
ens_preds <- predict(meta_model, newdata=testTransformed, type="prob")
model_preds$ensemble <- ens_preds
caTools::colAUC(model_preds, testTransformed$type)
