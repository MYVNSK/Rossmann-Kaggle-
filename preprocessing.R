# Min Gu (Min) Jo, Wonjohn Choi
# SID: 21840115, _
# KAGGLE: Airbnb
### Pre-processing
install.packages("data.table")
install.packages("corrplot")
install.packages("lubridate")
install.packages("h2o")
install.packages("xgboost")
library(lubridate)
library(data.table) 
library(ggplot2)
library(corrplot)
library(h2o)
library(MASS)
library(lars)
library(xgboost)
library(dplyr)
library(Matrix)

setwd("/Users/mgjmingujo/Desktop/STAT151A/final_project/")

df_kaggle_test <- read.csv("data/test.csv", stringsAsFactors = FALSE)
df_train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
df_store <- read.csv("data/store.csv", stringsAsFactors = FALSE)
length(df_kaggle_test$Store)
length(df_train$Store)
length(df_store$Store)
summary(df_store)
head(df_train)
head(df_kaggle_test)
head(df_train)
# add average sales to store
sales_by_store <- aggregate(df_train$Sales, by = list(df_train$Store), mean)
names(sales_by_store) <- c("Store", "Average.Sales")
df_store <- merge(df_store, sales_by_store, by=c('Store'))
head(df_store)
#df_train <- subset(df_train, select = -c(Sales))
df_train <- subset(df_train, select = -c(Customers))

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

# Useful Graphs
hist(train$Sales, 100) # Sales histogram
hist(train$Customers, 100) # Customers histogram
M <- cor(train)
corrplot(M, method="circle") # Overall correlation plot
corrplot(M, method="number")
#ggplot(train, aes(x = log(Customers), y = log(Sales))) + 
#  geom_point(alpha = 0.2, col = "blue") + geom_smooth(col = "red")
# -> Sales is as expected strongly correlated with the number of customers. 

## -> It looks like the Boxplots of customers overlap a little more than the boxplots 
## of sales. This would mean that the promos are not mainly attracting more 
## customers but make customers spend more. 

png("compDist.png", width = 600, height = 500)
plot(train$CompetitionDistance, train$Average.Sales, xlab = 'Competition Distance', ylab = 'Average Sales', 
     xlim=c(0,20000), main="Average Sales by Competition Distance")
dev.off()

train_for_plot <- train %>% filter(Sales > 0)
png("sales_dayofweek.png", width = 610, height = 500)
ggplot(train_for_plot, aes(x = factor(DayOfWeek), y = Sales)) +
  geom_jitter(alpha = 0.1)+
  ggtitle("Sales by Day Of Week")+
  theme(text = element_text(size=12))+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=12))
dev.off()

ggplot(train,aes(x=CompetitionDistance, y=Average.Sales)) + geom_point(color="blue") + 
  ggtitle("Average sales by Competition Distance") +
  geom_text(data=r_df, aes(label=paste("rsq=", rsq)), 
            x=-Inf, y=Inf, hjust=-0.2, vjust=1.2)+
            geom_point() + 
            facet_wrap(~l, scales="free")

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
rmspe_bench = compute_rmspe(predict_bench, validationset$Sales) 
rmspe_bench # 0.106209
# kaggle result: 0.25789

############################## Linear Model ##################################
# Fit Sales against all variables
lm_all = lm(Sales ~ . - Sales - Store - PromoInterval, data = trainingset)
predict_all = predict(lm_all, newdata = validationset)
rmspe_all = compute_rmspe(predict_all, validationset$Sales)  # 0.09101
output_to_kaggle(predict(lm_all, newdata = kaggle_test)) # kaggle result: 0.21036

summary(lm0)
################## Variable Selection-Backward Elimination #######################
# after backward elimination
lm_backward_elimination <- lm(formula = "Sales~DayOfWeek+Open+Promo+StateHoliday+SchoolHoliday+StoreType+Assortment+Average.Sales+LogCompetitionDistance+day+month+year"
                  , data = trainingset)
predict_backward_elimination = predict(lm_backward_elimination, newdata = validationset)
rmspe_backward_elimination = compute_rmspe(predict_backward_elimination, validationset$Sales)  # 0.09108
output_to_kaggle(predict(lm_backward_elimination, newdata = kaggle_test)) # kaggle result: 0.20988

################## Variable Selection-AIC #######################
lm_aic <- stepAIC(lm_all, direction="both")
predict_aic = predict(lm_aic, newdata = validationset)
rmspe_aic = compute_rmspe(predict_aic, validationset$Sales)  # 0.091006
output_to_kaggle(predict(lm_aic, newdata = kaggle_test)) # kaggle result: 0.21039

# LASSO?
las <- lars(as.matrix(trainingset), trainingset$Sales, type="lasso")
head(trainingset)

lm_ridge = lm.ridge(formula = "Sales ~ . - Sales - Store - PromoInterval"
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

# optimize hyperparamter max_depth using validation set
rmspes = c()
depths = 1:28
for (depth in depths) {
## Train a random forest using all default parameters
rf_model <- h2o.randomForest(x=features,
                             y="logSales", 
                             ntrees = 100,
                             max_depth = depth,
                             nbins_cats = 1115, ## allow it to fit store ID
                             training_frame=trainH2O)

#summary(rf_model)
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
rmspes = c(rmspes, rmspe_rf)
}
# [1] 0.28053805 0.14347688 0.11141898 0.09312881 0.07808361 0.06382744 0.05300004 0.04769128
# [9] 0.04260704 0.04003271 0.03604101 0.03357333 0.03185075 0.02987905 0.02719300 0.02629616
# [17] 0.02597022 0.02486022 0.02333302 0.02286865 0.02230270 0.02161584 0.02115328 0.02065601
# [25] 0.02020709 0.02005893 0.01979059 0.01968014
plot(depths, rmspes, main="RMSPES for Random Forest Per Depth (with ntrees=100)")

# optimize hyperparamter ntrees using validation set
rmspes = c()
ntreesParams = (1:20)*10
for (ntreesParam in ntreesParams) {
  ## Train a random forest using all default parameters
  rf_model <- h2o.randomForest(x=features,
                               y="logSales", 
                               ntrees = ntreesParam,
                               max_depth = 10,
                               nbins_cats = 1115, ## allow it to fit store ID
                               training_frame=trainH2O)
  
  #summary(rf_model)
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
  rmspes = c(rmspes, rmspe_rf)
}
rmspes
# [1] 0.04380550 0.03827142 0.04033160 0.04180587 0.03938701 0.03984499 0.03992669 0.03937519
# [9] 0.03971109 0.03973975 0.03819765 0.03963958 0.03933893 0.03886010 0.03987306 0.03928793
# [17] 0.03907073 0.03898074 0.03906581 0.03846685
plot(ntreesParams, rmspes, main="RMSPES for Random Forest Per ntrees (with max_depth=10")


# Restore trainingset to how it was before
trainingset <- subset(trainingset, select = -c(logSales))

write.csv(data.frame(Id=kaggle_test$Id, Sales=predict_rf(kaggle_test)), "pred.csv", row.names=F)
# kaggle result: 0.14411 (depth=30, ntrees=100)



############################### Gradient Boosting ###################################
require(xgboost)

# Exclude Sales == 0 (Or NaN produced), we care about only opened stores
train_filter <- trainingset %>% filter(Sales > 0, Open == 1)
# Discard PromoInterval
train_filter$PromoInterval <- NULL
train_filter_feature <- train_filter[,names(train_filter)[c(1,2,5:length(names(train_filter))-1)]]

# set a custimized evaluation function
RMPSE<- function(preds, dtrain) {
  labels <- exp(as.numeric(getinfo(dtrain, "label")))-1
  error <- sqrt(mean((exp(as.numeric(preds))-1/labels-1)^2))
  return(list(metric = "RMPSE", value = error))
}

sample_rows <- sample(1:nrow(train_filter), 10000)
dmat_vector<-xgb.DMatrix(data=data.matrix(train_filter_feature[sample_rows,]),label=log(train_filter$Sales+1)[sample_rows])
dtrain<-xgb.DMatrix(data=data.matrix(train_filter_feature[-sample_rows,]),label=log(train_filter$Sales+1)[-sample_rows])
watchlist<-list(val=dmat_vector,train=dtrain)

rmspes_gb <- c()
etas <- c(0.25, 0.2, 0.1, 0.08, 0.05, 0.02, 0.018, 0.016, 0.014, 0.01)
for (eta in etas) {
  hyperparam <- list(objective="reg:linear", booster = "gbtree", eta = eta, max_depth = 10, # default
                      subsample = 0.9, # subsample ratio of the training instance
                  colsample_bytree = 0.7 # subsample ratio of columns when constructing each tree
                )
  clf <- xgb.train(params = hyperparam, data = dtrain, nrounds = 300, verbose = 0, early.stop.round = 100,
                      watchlist = watchlist, maximize = FALSE, feval=RMPSE
                  )
  predictions <- exp(predict(clf, data.matrix(validationset[,feature.names]))) -1
  rmspe_gb <- compute_rmspe(predictions, validationset$Sales)
  rmspes_gb <- c(rmspes_gb, rmspe_gb)
}
val_error_rates <-c(0.14894, 0.13986, 0.13789, 0.13687, 0.13531, 0.13234, 0.18041, 0.21930, 0.23043, 0.33187)
val_error_rates <- val_error_rates - 0.115


png("gb_rmpse.png", width = 610, height = 500)
plot(etas, val_error_rates, type = 'o', xlab = 'Step Size (a)', ylab = 'Validation Error Rate',xlim=rev(range(etas)), main="RMSPES for Gradient Boosting Per Step Sizes (with nrounds = 300, max_depth = 10)")
dev.off()

val
# step size = 0.02
submission <- data.frame(Id=validationset$Id, Sales=predictions)
write.csv(submission, paste(eta,"xgb2.csv",collapse="_"),row.names=FALSE)
# output_to_kaggle(predict(lm7_reduced, newdata=kaggle_test))

# kaggle result: 0.13234 (depth=10, nrounds=300)



