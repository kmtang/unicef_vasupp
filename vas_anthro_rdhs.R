
# Example rdhs file

# Use this version, because there are a few errors that have not been resolved in the cran version yet
devtools::install_github("ropensci/rdhs", ref = "issue33_path")
library(rdhs)

devtools::install_github("mrc-ide/demogsurv")
library(demogsurv)
library(data.table)
library(haven)
library(tidyverse)
library(dplyr)
options(stringsAsFactors = TRUE)

# Download the DHS data

## set up your credentials
set_rdhs_config(email = "kevin.tang1@student.lshtm.ac.uk",
                password_prompt =  TRUE,
                project = "Micronutrient Action Policy Support Tool", # Put your project name here
                config_path = "rdhs.json",
                global=FALSE)

# we will give our permission here so that we don't have to provide a prompt within the README.
Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)

## a little nugget to return API requests as data.table rather than data.frame.
Sys.setenv(rdhs_DATA_TABLE = "TRUE")

## Identify all DHS surveys for the following countries
countries <- dhs_countries()
cc <- c( "ET",  "MW", "CM", "BF", "BJ", "CF", "MD", "ML", "NP", "NI", "PK", "ZA")

# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=2000, surveyType = "DHS")

# Identify births recode (BR) datasets corresponding to these surveys.
step <- dhs_datasets(fileType = "IR", fileFormat = "flat")
ird <- step[which(step$SurveyId %in% surveys$SurveyId),]

# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
Sys.setenv("rdhs_LOUD_DOWNLOAD" = TRUE)
ird$path <- unlist(get_datasets(ird$FileName,clear_cache = TRUE))

# Load all of the datasets into R as a list.
ir <- list()
for(survid in ird$SurveyId){
  print(survid)
  dat <- readRDS(ird[which(ird$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|^b8_|^b19_|^hc|^hw1_|^hw2_|^hw3_|^hw70_|^hw71_|^hw72_|^h33_", names(dat))]
  ir[[survid]] <- dat
}

ir <- lapply(ir, haven::as_factor)

##Add survey-level variables

ir <- Map(data.frame,
          SurveyId = surveys$SurveyId,
          CountryName = surveys$CountryName,
          SurveyYear = surveys$SurveyYear,
          ir)

ir <- lapply(ir, function(x){ x$SurveyId <- factor(x$SurveyId) ; return(x)})
ir <- lapply(ir, function(x){ x$CountryName <- factor(x$CountryName) ; return(x)})
ir <- lapply(ir, function(x){ x$SurveyYear <- factor(x$SurveyYear) ; return(x)})

#Vitamin A
data <- ir$MW2015DHS

c1 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_01, b19_01, h33_1, hw1_1, hw2_1, hw3_1, hw70_1, hw71_1, hw72_1)
c2 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_02, b19_02, h33_2, hw1_2, hw2_2, hw3_2, hw70_2, hw71_2, hw72_2)
c3 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_03, b19_03, h33_3, hw1_3, hw2_3, hw3_3, hw70_3, hw71_3, hw72_3)
c4 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_04, b19_04, h33_4, hw1_4, hw2_4, hw3_4, hw70_4, hw71_4, hw72_4)
c5 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_05, b19_05, h33_5, hw1_5, hw2_5, hw3_5, hw70_5, hw71_5, hw72_5)
c6 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_06, b19_06, h33_6, hw1_6, hw2_6, hw3_6, hw70_6, hw71_6, hw72_6)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'age_m','vas','age_m2', 'weight', 'height', 'wa_sd', 'ha_sd', 'wh_sd')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'age_m','vas','age_m2', 'weight', 'height', 'wa_sd', 'ha_sd', 'wh_sd')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'age_m','vas','age_m2', 'weight', 'height', 'wa_sd', 'ha_sd', 'wh_sd')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'age_m','vas','age_m2', 'weight', 'height', 'wa_sd', 'ha_sd', 'wh_sd')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'age_m','vas','age_m2', 'weight', 'height', 'wa_sd', 'ha_sd', 'wh_sd')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'age_m','vas','age_m2', 'weight', 'height', 'wa_sd', 'ha_sd', 'wh_sd')

c1 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_01, h33_1, hw70_1, hw71_1, hw72_1)
c2 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_02, h33_2, hw70_2, hw71_2, hw72_2)
c3 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_03, h33_3, hw70_3, hw71_3, hw72_3)
c4 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_04, h33_4, hw70_4, hw71_4, hw72_4)
c5 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_05, h33_5, hw70_5, hw71_5, hw72_5)
c6 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, b8_06, h33_6, hw70_6, hw71_6, hw72_6)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'vas', 'wa_sd', 'ha_sd', 'wh_sd')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'vas', 'wa_sd', 'ha_sd', 'wh_sd')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'vas', 'wa_sd', 'ha_sd', 'wh_sd')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'vas', 'wa_sd', 'ha_sd', 'wh_sd')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'vas', 'wa_sd', 'ha_sd', 'wh_sd')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','age', 'vas', 'wa_sd', 'ha_sd', 'wh_sd')

data <- rbind(c1,c2,c3,c4,c5,c6)

data <-data %>% filter(age_m>5) %>% filter(age_m<60)
data <-data %>% filter(age>0) %>% filter(age<6)

data <- data %>% mutate(vasupp = case_when(vas=="vaccination date on card" ~ 1,
                                           vas=="reported by mother" ~ 1,
                                           vas=="vaccination marked on card" ~ 1,
                                           vas=="don't know" ~ 2,
                                           vas=="missing" ~ 2,
                                           vas=="no" ~ 2))

#Is child wasted?
data$wh_sd <- as.character(data$wh_sd)
data$wh_sd <- as.numeric(data$wh_sd)

data <- data %>% mutate(wasted = if_else(wh_sd < -200, "wasted", "not wasted"))

data %>% filter(!is.na(vasupp)) %>% filter(!is.na(wasted)) %>% nrow()

table <- table(data$vasupp, data$wasted)
table
prop.table(table, 2)
prop.test(420,(420+213),correct=FALSE)

##Is child underweight?
data$wa_sd <- as.character(data$wa_sd)
data$wa_sd <- as.numeric(data$wa_sd)

data <- data %>% mutate(underweight = if_else(wa_sd < -200, "underweight", "not underweight"))

data %>% filter(!is.na(vasupp)) %>% filter(!is.na(underweight)) %>% nrow()

table <- table(data$vasupp, data$underweight)
table
prop.table(table, 2)
prop.test(1017,(1017+298),correct=FALSE)

## Is child stunted
data$ha_sd <- as.character(data$ha_sd)
data$ha_sd <- as.numeric(data$ha_sd)

data <- data %>% mutate(stunted = if_else(ha_sd < -200, "stunted", "not stunted"))

data %>% filter(!is.na(vasupp)) %>% filter(!is.na(stunted)) %>% nrow()

table <- table(data$vasupp, data$stunted)
table
prop.table(table, 2)
prop.test(909,(909 +510),correct=FALSE)
