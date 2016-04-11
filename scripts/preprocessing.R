# Min Gu (Min) Jo
# SID: 21840115
# KAGGLE: Airbnb
### Pre-processing
install.packages("lubridate")
install.packages("dplyr")
library(lubridate)
library(stringr)
library(Hmisc)
library(dplyr)

setwd("/Users/mgjmingujo/Desktop/STAT151A/final_project/")

# Load the data
df_train <- read.csv("data/train_users_2.csv", stringsAsFactors = FALSE)
df_test <- read.csv("data/test_users.csv", stringsAsFactors = FALSE)
age_gender_bkts <- read.csv("./data/age_gender_bkts.csv", stringsAsFactors = FALSE)
countries <- read.csv("./data/countries.csv", stringsAsFactors = FALSE)
sessions <- read.csv("./data/sessions.csv", stringsAsFactors = FALSE)

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
age_group <- matrix(0, nrow = 21, ncol=2)
age_group[,1] <- seq(0, 100, 5)
age_group[,2] <- seq(4, 104, 5)
age_group[21,2] <- 150
age_group <- data.frame(age_group)
names(age_group) <- c("Lower", "Upper")

getAge_Range <- function(x) {
  paste(age_group[x,1], "-", age_group[x,2], sep = "")
}
age_group$age_range <- sapply(1:nrow(age_group), getAge_Range)
age_group$age_range[21] <- "100+"

##### Create more features with date_account_created
df_all$date_account_created = ymd(df_all$date_account_created)
df_all$dac_yr = year(df_all$date_account_created)     # new feature
df_all$dac_mth = month(df_all$date_account_created)      # new feature
df_all$dac_day = day(df_all$date_account_created)     # new feature
df_all$dac_yr_mth_day = as.numeric(paste0(df_all$dac_yr, df_all$dac_mth, df_all$dac_day))     # new feature
df_all$dac_wk = as.numeric(format(df_all$date_account_created+3, "%U"))    ### add the missing 3 days to the following month, March
df_all$date_account_created <- NULL

##### Create more features with timestamp_first_active
df_all$timestamp_first_active = as.character(df_all$timestamp_first_active)
df_all$tfa_yr = as.numeric(str_sub(df_all$timestamp_first_active, 1, 4))     # new feature
df_all$tfa_mth = as.numeric(str_sub(df_all$timestamp_first_active, 5, 6))     # new feature
df_all$tfa_day = as.numeric(str_sub(df_all$timestamp_first_active, 7, 8))     # new feature
df_all$tfa_hr = as.numeric(str_sub(df_all$timestamp_first_active, 9, 10))     # new feature
df_all$tfa_yr_mth_day_hr = as.numeric(str_sub(df_all$timestamp_first_active, 1, 10))     # new feature
df_all$tfa_date = as.Date(paste(df_all$tfa_yr, df_all$tfa_mth, df_all$tfa_day, sep="-"))    # new feature
df_all$tfa_wk = as.numeric(format(df_all$tfa_date+3, "%U"))     # new feature
df_all$timestamp_first_active <- NULL

#### Add a feature of lag from account creation to first active time (difference in days)
df_all$lag = as.numeric(df_all$date_account_created - df_all$tfa_date)

##### Create more features with date_first_booking
df_all$date_first_booking = ymd(df_all$date_first_booking)
df_all$dfb_yr = year(df_all$date_first_booking)     # new feature
df_all$dfb_mth = month(df_all$date_first_booking)      # new feature
df_all$dfb_day = day(df_all$date_first_booking)     # new feature
df_all$dfb_yr_mth_day = as.numeric(paste0(df_all$dfb_yr, df_all$dfb_mth, df_all$dfb_day))     # new feature
df_all$dfb_tfa_lag = as.numeric(df_all$date_first_booking - df_all$tfa_date)

##### Change string type features to factors (numeric)
df_all$gender <- as.factor(df_all$gender)
df_all$signup_method <- as.factor(df_all$signup_method)
df_all$signup_method <- as.factor(df_all$signup_method)
#df_all$signup_flow <- as.factor(df_all$signup_flow)
df_all$language <- as.factor(df_all$language)
df_all$affiliate_channel <- as.factor(df_all$affiliate_channel)
df_all$affiliate_provider <- as.factor(df_all$affiliate_provider)
df_all$first_affiliate_tracked <- as.factor(df_all$first_affiliate_tracked)
df_all$signup_app <- as.factor(df_all$signup_app)
df_all$first_device_type <- as.factor(df_all$first_device_type)
df_all$first_browser <- as.factor(df_all$first_browser)

##### Manipulate the feature "age" and add age_group range
df_all$age[is.na(df_all$age) | df_all$age > 120] <- -1
group_idx <- ceiling((df_all$age+1)/5)
group_idx[group_idx > dim(age_group)[1]] <- dim(age_group)[1]
df_all$age_grp <- NA
df_all$age_grp[group_idx > 0] <- age_group$age_range[group_idx]
df_all$age[df_all$age == -1] <- NA

##### Join country information
countries$language <- str_sub(countries$destination_language, 1, 2)
df_all <- left_join(df_all, countries[c("country_destination", "language", "distance_km", 
                                  "destination_km2", "language_levenshtein_distance")],
                                  by = c("country_destination", "language"))

#### stack categorical and numeric features???


#### Work on seessions data
a=table(sessions$user_id)
all_usrs <- merge(df_all, sessions, all.x = TRUE, by.x = "id", by.y = "user_id")


