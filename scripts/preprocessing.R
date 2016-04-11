# Min Gu (Min) Jo
# SID: 21840115
# KAGGLE: Airbnb
### Pre-processing

setwd("/Users/mgjmingujo/Desktop/STAT151A/final_project/")

# Load the data
df_train <- read.csv("data/train_users_2.csv", stringsAsFactors = FALSE)
df_test <- read.csv("data/test_users.csv", stringsAsFactors = FALSE)
age_gender_bkts <- read.csv("./data/age_gender_bkts.csv", stringsAsFactors = FALSE)
countries <- read.csv("./data/countries.csv", stringsAsFactors = FALSE)

# add destination column in test data and fill up with "N/A"
df_test$country_destination <- "N/A"

# combine train and test dataset
df_all <- rbind(df_train, df_test)
df_all <- df_all[, c(1, 16, 2:15)]

# cheeck length 
dim(df_train)
dim(df_test)
dim(df_all)

# Check the data and column names
head(df_train)
head(df_test)
head(df_all)
head(age_gender_bkts)
head(countries)

# plot age_gender distribution (barplot)
plot(age_gender_bkts)

# clean age
age_groups <- matrix(0, nrow = 21, ncol=2)
age_groups[,1] <- seq(0, 100, 5)
age_groups[,2] <- seq(4, 104, 5)
age_groups[21,2] <- 150
age_groups <- data.frame(age_groups)
names(age_groups) <- c("Lower", "Upper")

getCharRep <- function(ii) {
  paste(age_groups[ii,1], "-", age_groups[ii,2], sep = "")
}
age_groups$CharRep <- sapply(1:nrow(age_groups), getCharRep)
age_groups$CharRep[21] <- "100+"

##### Create more features using date_account_created, timestamp_first_active, date_first_booking
dac_yearmonth = paste0(dac_year, dac_month),
dac_yearmonthday = as.numeric(paste0(dac_year, dac_month, dac_day)),
dac_week = as.numeric(format(date_account_created+3, "%U")),
dac_yearmonthweek = as.numeric(paste0(dac_year, dac_month, formatC(dac_week, width=2, flag="0"))),
tfa_year = str_sub(timestamp_first_active, 1, 4),
tfa_month = str_sub(timestamp_first_active, 5, 6),
tfa_day = str_sub(timestamp_first_active, 7, 8),
tfa_yearmonth = str_sub(timestamp_first_active, 1, 6),
tfa_yearmonthday = as.numeric(str_sub(timestamp_first_active, 1, 8)),
tfa_date = as.Date(paste(tfa_year, tfa_month, tfa_day, sep="-")),
tfa_week = as.numeric(format(tfa_date+3, "%U")),
tfa_yearmonthweek = as.numeric(paste0(tfa_year, tfa_month, formatC(tfa_week, width=2, flag="0"))),
dac_lag = as.numeric(date_account_created - tfa_date),
dfb_dac_lag = as.numeric(date_first_booking - date_account_created),
dfb_dac_lag_cut = as.character(cut2(dfb_dac_lag, c(0, 1))),
dfb_dac_lag_flg = as.numeric(as.factor(ifelse(is.na(dfb_dac_lag_cut)==T, "NA", dfb_dac_lag_cut))) - 1,
dfb_tfa_lag = as.numeric(date_first_booking - tfa_date),
dfb_tfa_lag_cut = as.character(cut2(dfb_tfa_lag, c(0, 1))),
dfb_tfa_lag_flg = as.numeric(as.factor(ifelse(is.na(dfb_tfa_lag_cut)==T, "NA", dfb_tfa_lag_cut))) - 1






