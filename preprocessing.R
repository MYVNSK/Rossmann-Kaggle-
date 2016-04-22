# Min Gu (Min) Jo, Wonjohn Choi
# SID: 21840115, _
# KAGGLE: Airbnb
### Pre-processing
install.packages("data.table")
install.packages("corrplot")
install.packages("lubridate")
install.packages("h2o")
library(lubridate)
library(data.table) 
library(ggplot2)
library(corrplot)
library(h2o)
library(MASS)
library(lars)


setwd("/Users/mgjmingujo/Desktop/STAT151A/final_project/")

df_kaggle_test <- read.csv("data/test.csv", stringsAsFactors = FALSE)
df_train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
df_store <- read.csv("data/store.csv", stringsAsFactors = FALSE)

# add average sales to store
sales_by_store <- aggregate(df_train$Sales, by = list(df_train$Store), mean)
names(sales_by_store) <- c("Store", "Average.Sales")
df_store <- merge(df_store, sales_by_store, by=c('Store'))
head(df_store)
#df_train <- subset(df_train, select = -c(Sales))
df_train <- subset(df_train, select = -c(Customers))
head(df_test)

# clean df_store
# impute Competition Values
df_store$CompetitionOpenSinceYear[is.na(df_store$CompetitionOpenSinceYear)] <- 1990 # Dealing with NA and outlayers
df_store$CompetitionOpenSinceMonth[is.na(df_store$CompetitionOpenSinceMonth)] <- 1 # Dealing with NA
df_store$CompetitionDistance[is.na(df_store$CompetitionDistance)] <- 1000000 # Dealing with NA.
# This value is much larger than max of given competition distances (75860)
df_store$LogCompetitionDistance <- log1p(df_store$CompetitionDistance)
df_store$StoreType <- as.factor(df_store$StoreType)
df_store$Assortment <- as.factor(df_store$Assortment)
df_store$Promo2 <- as.factor(df_store$Promo2)
df_store$CompetitionOpenSinceMonth <- as.factor(df_store$CompetitionOpenSinceMonth)
df_store$CompetitionOpenSinceYear <- as.factor(df_store$CompetitionOpenSinceYear)

which(is.na(df_train$Promo2SinceYear) & df_train$Promo2 == 1) # integer(0)
# This means that if Promo2 == 0, Promo2SinceYear or Promo2SinceWeek can be NA.
df_store$Promo2SinceYear[is.na(df_store$Promo2SinceYear)] <- 1990 # Dealing with NA
df_store$Promo2SinceWeek[is.na(df_store$Promo2SinceWeek)] <- 1 # Dealing with NA
df_store$Promo2 <- as.factor(df_store$Promo2)
head(df_store)


clean <- function(df_train) {
  # Merge df_store into df_train
  train <- merge(df_train, df_store, by=c('Store'))

  # Exclude closed shops from training
  # train <- train[which(train$Open==1),]
  # train <- subset(train, select = -c(Open))

  # 54 shops in the train set are open but have no sales, so exclude
  # train <- train[which(train$Sales>0),]

  # add year, mth, date, day of week features
  train$Store <- as.factor(train$Store)
  
  train$Date <- as.Date(train$Date)
  train$day <- as.factor(format(train$Date, "%d"))
  train$month <- as.factor(format(train$Date, "%m"))
  train$year <- as.factor(format(train$Date, "%Y"))
  train <- subset(train, select = -c(Date))

  train$DayOfWeek <- as.factor(train$DayOfWeek)

  ### Factorize
  train$Open <- as.factor(train$Open)
  train$Promo <- as.factor(train$Promo)
  train$StateHoliday <- as.factor(train$StateHoliday)
  train$SchoolHoliday <- as.factor(train$SchoolHoliday)
  return(train)
}

train = clean(df_train)
kaggle_test = clean(df_kaggle_test)
kaggle_test$Open[is.na(kaggle_test$Open)] = 0
head(train)
head(kaggle_test)


# Useful Graphs
#hist(train$Sales, 100) # Sales histogram
#hist(train$Customers, 100) # Customers histogram
#M <- cor(train)
#corrplot(M, method="circle") # Overall correlation plot
#corrplot(M, method="number")
#ggplot(train, aes(x = log(Customers), y = log(Sales))) + 
#  geom_point(alpha = 0.2, col = "blue") + geom_smooth(col = "red")
## -> Sales is as expected strongly correlated with the number of customers. 

#ggplot(train, aes(x = factor(Promo), y = Customers)) + 
#  geom_jitter(alpha = 0.1, color = "gray") +
#  geom_boxplot(color = "red", outlier.colour = NA, fill = NA)

## -> It looks like the Boxplots of customers overlap a little more than the boxplots 
## of sales. This would mean that the promos are not mainly attracting more 
## customers but make customers spend more. 

#ggplot(train,aes(x=CompetitionDistance, y=Average.Sales)) + geom_point(color="blue") + 
#  ggtitle("Average sales by Competition Distance")

## They are negatively correlated. 

#zerosPerStore <- sort(tapply(train$Sales, list(train$Store), function(x) sum(x == 0)))
#hist(zerosPerStore,100)

#ggplot(train[train$Store == 262,], aes(x = Date, y = Sales, color = factor(DayOfWeek == 7))) + 
#  geom_point(size = 2) + ggtitle("Sunday vs. rest of day Sales of store 262 (True == sunday)") + scale_color_manual(values = c("red","blue"))

## -> There are also stores that have no zeros in their sales. These are the exception 
## since they are opened also on sundays / holidays. The sales of those stores on sundays are particularly high:

########################### Utility Functions ##############################
# rmspe
compute_rmspe <- function(predicted, expected) {
  predicted = predicted[expected != 0]
  expected = expected[expected != 0]
  mean(((predicted - expected) / expected)^2)
}

# Kaggle Output CSV (predicted using kaggle_test)
# Example usage: output_to_kaggle(predict(lm7_reduced, newdata=kaggle_test))
output_to_kaggle <- function(predicted) {
  write.csv(data.frame(Id=kaggle_test$Id, Sales=predicted), "pred.csv", row.names=F)
}

######################### Prepare Cross Validation #############################
k = 5 #Folds
set.seed(42)
# sample from 1 to k, nrow times (the number of observations in the data)
id <- sample(1:k, nrow(train), replace = TRUE)
list <- 1:k
trainingset <- subset(train, id %in% list[-1])
validationset <- subset(train, id %in% c(1))

############################### Benchmark ###################################
# Predicting using the average sales per store
predict_bench = validationset$Average.Sales
rmspe_bench = compute_rmspe(predict_bench, validationset$Sales)  # 0.106209
# kaggle result: 0.25789

############################## Linear Model ##################################

summary(trainingset)
lm1 = lm(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday, data = trainingset)
predict1 = predict(lm1, newdata = validationset)
rmspe1 = compute_rmspe(predict1, validationset$Sales)  # 0.27857

lm2 = lm(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday + StoreType, data = trainingset)
predict2 = predict(lm2, newdata = validationset)
rmspe2 = compute_rmspe(predict2, validationset$Sales)  # 0.27831

lm3 = lm(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday + StoreType + Assortment + Promo2, data = trainingset)
predict3 = predict(lm3, newdata = validationset)
rmspe3 = compute_rmspe(predict3, validationset$Sales)  # 0.26414

lm4 = lm(Sales ~ DayOfWeek + Promo + StateHoliday + SchoolHoliday + StoreType + Assortment + Promo2 + day + month + year, data = trainingset)
predict4 = predict(lm4, newdata = validationset)
rmspe4 = compute_rmspe(predict4, validationset$Sales)  # 0.25452

lm5 = lm(Sales ~ . - Average.Sales - Sales - Customers - Store - PromoInterval, data = trainingset)
predict5 = predict(lm5, newdata = validationset)
rmspe5 = compute_rmspe(predict5, validationset$Sales)  # 0.25257

lm6 = lm(Sales ~ . - Sales - Store - PromoInterval - LogCompetitionDistance, data = trainingset)
predict6 = predict(lm6, newdata = validationset)
rmspe6 = compute_rmspe(predict6, validationset$Sales)  # 0.09112

summary(lm7)
lm7 = lm(Sales ~ . - Sales - Store - PromoInterval - CompetitionDistance, data = trainingset)
predict7 = predict(lm7, newdata = validationset)
rmspe7 = compute_rmspe(predict7, validationset$Sales)  # 0.091003
# kaggle result: 0.21036
summary(lm7)

############################# Variable Selection ############################
lm7_aic <- stepAIC(lm7, direction="both")
summary(lm7_aic)
predict7_aic = predict(lm7_aic, newdata = validationset)
rmspe7_aic = compute_rmspe(predict7_aic, validationset$Sales)  # 0.091006
# kaggle result: 0.21039

# manual variable selection using p-value
summary(lm7)
lm7_reduced <- lm(formula = "Sales~DayOfWeek+Open+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+Promo2SinceWeek+Average.Sales+LogCompetitionDistance+day+month+year"
, data = trainingset)
predict7_reduced = predict(lm7_reduced, newdata = validationset)
rmspe7_reduced = compute_rmspe(predict7_reduced, validationset$Sales)  # 0.909679
# kaggle result: 0.21031

# LASSO?
las <- lars(as.matrix(trainingset), trainingset$Sales, type="lasso")
head(trainingset)

lm_ridge = lm.ridge(formula = "Sales~DayOfWeek+Open+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+Promo2SinceWeek+Average.Sales+LogCompetitionDistance+day+month+year"
         , data = trainingset)
coef(lm_ridge)
predict_ridge = predict(lm_ridge, newdata = validationset)
rmspe7_ridge = compute_rmspe(predict_ridge, validationset$Sales)  # 0.909679


# output_to_kaggle(predict(lm7_reduced, newdata=kaggle_test))

############################### Random Forest ###################################
trainingset$logSales <- log1p(trainingset$Sales)
# Use H2O's random forest
# start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')
# Load data into cluster from R
trainH2O<-as.h2o(trainingset)
## Set up variable to use all features other than those specified here
features<-colnames(trainingset)[!(colnames(trainingset) %in% c("Sales","logSales","PromoInterval", "CompetitionDistance"))]
## Train a random forest using all default parameters
rf_model <- h2o.randomForest(x=features,
                             y="logSales", 
                             ntrees = 100,
                             max_depth = 30,
                             nbins_cats = 1115, ## allow it to fit store ID
                             training_frame=trainH2O)
# Restore trainingset to how it was before
trainingset <- subset(trainingset, select = -c(logSales))

summary(rf_model)
predict_rf <- function(dataset) {
  # Load test data into cluster from R
  testH2O<-as.h2o(dataset)
  # Get predictions out; predicts in H2O, as.data.frame gets them into R
  predictions<-as.data.frame(h2o.predict(rf_model,testH2O))
  # Return the predictions to the original scale of the Sales data
  pred <- expm1(predictions[,1])
  return (pred)
}
predicted_rf = predict_rf(validationset)
rmspe_rf = compute_rmspe(predicted_rf, validationset$Sales) # 0.020639

write.csv(data.frame(Id=kaggle_test$Id, Sales=predict_rf(kaggle_test)), "pred.csv", row.names=F)
# kaggle result: 0.14411




# GLM
trainingset$logSales <- log1p(trainingset$Sales)
## Use H2O's gradient boost machine 
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
trainH2O<-as.h2o(trainingset)
## Set up variable to use all features other than those specified here
features<-colnames(trainingset)[!(colnames(trainingset) %in% c("Sales","logSales","PromoInterval", "CompetitionDistance"))]

## Train a generalized linear model using all default parameters
glmHex <- h2o.gbm(x=features,
                  y="logSales",
                  training_frame=trainH2O,
                  family="gaussian",
                  nfolds = 0,
                  alpha = 0.5,   # 1 = lasso penalty, 0 = ridge penalty
                  lambda_search = FALSE,
                  use_all_factor_levels = FALSE,
                  higher_accuracy = FALSE,
                  return_all_lambda = FALSE
)
trainingset <- subset(trainingset, select = -c(logSales))

summary(glmHex)
## Load test data into cluster from R
testHex<-as.h2o(test)
## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(glmHex,testHex))
## Return the predictions to the original scale of the Sales data
pred <- expm1(predictions[,1])
summary(pred)