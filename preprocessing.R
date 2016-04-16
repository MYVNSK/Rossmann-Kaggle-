# Min Gu (Min) Jo
# SID: 21840115
# KAGGLE: Airbnb
### Pre-processing
install.packages("data.table")
library(lubridate)
library(data.table) 
library(ggplot2)


setwd("/Users/mgjmingujo/Desktop/STAT151A/final_project/")

df_train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
df_test <- read.csv("data/test.csv", stringsAsFactors = FALSE)
df_store <- read.csv("data/store.csv", stringsAsFactors = FALSE)

# Merge df_store into df_train
train <- merge(df_train, df_store, by=c('Store'))
test <- merge(df_test, df_store, by=c('Store'))

# Exclude closed shops from training
train <- train[which(Open==1),]

# 54 shops in the train set are open but have no sales, so exclude
train <- train[which(train$Sales>0),]

# Useless features????
useless_features = {'Date','Promo2SinceWeek','PromoInterval',
                    'CompetitionOpenSinceYear','CompetitionOpenSinceMonth','Promo2SinceYear'}


train$Date <- ymd(train$Date)
test$Date <- ymd(test$Date)
train$day <- as.factor(day(train$Date))
test$day <- as.factor(day(test$Date))
train$month <- as.factor(month(train$Date))
test$month <- as.factor(month(test$Date))
train$year <- as.factor(year(train$Date))
test$year <- as.factor(year(test$Date))
#train$Date <- NULL

train$DayOfWeek <- as.factor(train$DayOfWeek)
test$DayOfWeek <- as.factor(test$DayOfWeek)

### Factorize
train$Open <- as.factor(train$Open)
test$Open <- as.factor(test$Open)
train$Promo <- as.factor(train$Promo)
test$Promo <- as.factor(test$Promo)

train$SchoolHoliday <- as.factor(train$SchoolHoliday)
test$SchoolHoliday <- as.factor(test$SchoolHoliday)

train$StoreType <- as.factor(train$StoreType)
test$StoreType <- as.factor(test$StoreType)
train$Assortment <- as.factor(train$Assortment)
test$Assortment <- as.factor(test$Assortment)
train$CompetitionDistance <- as.numeric(train$CompetitionDistance)
test$CompetitionDistance <- as.numeric(test$CompetitionDistance)
train$Promo2 <- as.factor(train$Promo2)
test$Promo2 <- as.factor(test$Promo2)
train$PromoInterval <- as.factor(train$PromoInterval)
test$PromoInterval <- as.factor(test$PromoInterval)


# add average sales
df_sales_by_store <- aggregate(train$Sales,by = list(train$Store), mean)
names(df_sales_by_store) <- c("Store","Average.Sales")

train[is.na(train)] <- 0
test[is.na(test)] <- 0

# Check the data and column names
head(train)
head(test)


test[, Promo2Since:=Promo2SinceYear+Promo2SinceWeek/52]

