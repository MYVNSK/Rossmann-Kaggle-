# Min Gu (Min) Jo, Wonjohn Choi
# SID: 21840115, _
# KAGGLE: Airbnb
### Pre-processing
install.packages("data.table")
library(lubridate)
library(data.table) 
library(ggplot2)
library(corrplot)


setwd("/Users/mgjmingujo/Desktop/STAT151A/final_project/")

df_train <- read.csv("data/train.csv", stringsAsFactors = FALSE)
df_test <- read.csv("data/test.csv", stringsAsFactors = FALSE)
df_store <- read.csv("data/store.csv", stringsAsFactors = FALSE)

# Merge df_store into df_train
train <- merge(df_train, df_store, by=c('Store'))
test <- merge(df_test, df_store, by=c('Store'))

# Exclude closed shops from training
train <- train[which(Open==1),]
train <- subset(train, select = -c(Open))

# 54 shops in the train set are open but have no sales, so exclude
train <- train[which(train$Sales>0),]

# add year, mth, date, day of week features
train$Date <- ymd(train$Date)
test$Date <- ymd(test$Date)
train$day <- as.numeric(as.factor(day(train$Date)))
test$day <- as.numeric(as.factor(day(test$Date)))
train$month <- as.numeric(as.factor(month(train$Date)))
test$month <- as.numeric(as.factor(month(test$Date)))
train$year <- as.numeric(as.factor(year(train$Date)))
test$year <- as.numeric(as.factor(year(test$Date)))

train$Date <- NULL ######

train$DayOfWeek <- as.numeric(as.factor(train$DayOfWeek))
test$DayOfWeek <- as.numeric(as.factor(test$DayOfWeek))

### Factorize
train$Promo <- as.numeric(as.factor(train$Promo))
test$Promo <- as.numeric(as.factor(test$Promo))
train$StateHoliday <- as.numeric(as.factor(train$StateHoliday))
test$StateHoliday <- as.numeric(as.factor(test$StateHoliday))
train$SchoolHoliday <- as.numeric(as.factor(train$SchoolHoliday))
test$SchoolHoliday <- as.numeric(as.factor(test$SchoolHoliday))
train$StoreType <- as.numeric(as.factor(train$StoreType))
test$StoreType <- as.numeric(as.factor(test$StoreType))
train$Assortment <- as.numeric(as.factor(train$Assortment))
test$Assortment <- as.numeric(as.factor(test$Assortment))
train$CompetitionDistance <- as.numeric(train$CompetitionDistance)
test$CompetitionDistance <- as.numeric(test$CompetitionDistance)
train$Promo2 <- as.numeric(as.factor(train$Promo2))
test$Promo2 <- as.numeric(as.factor(test$Promo2))
train$PromoInterval <- as.numeric(as.factor(train$PromoInterval))
test$PromoInterval <- as.numeric(as.factor(test$PromoInterval))

# add average sales
df_sales_by_store <- aggregate(train$Sales,by = list(train$Store), mean)
names(df_sales_by_store) <- c("Store","Average.Sales")
train <- merge(train, df_sales_by_store, by=c('Store'))
test <- merge(test, df_sales_by_store, by=c('Store'))


# fill 'NA' with 0
train[is.na(train)] <- 1
test[is.na(test)] <- 0

# Check the data and column names
head(train)
head(test)

# Useful Graphs
hist(train$Sales, 100) # Sales histogram
hist(train$Customers, 100) # Customers histogram
M <- cor(train)
corrplot(M, method="circle") # Overall correlation plot
corrplot(M, method="number")




### Linear Model
train_temp <- subset(train, select = -c(Open) )


train_omit <- na.omit(train)
lm_1 <- lm(Sales ~ . , data = train_temp)

# Take out useless features according to the linear model
train <- subset(train, select = -c(Promo2SinceWeek, Promo2SinceYear, CompetitionOpenSinceYear, CompetitionOpenSinceMonth) )


