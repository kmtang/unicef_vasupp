
# Example rdhs file

# Use this version, because there are a few errors that have not been resolved in the cran version yet
devtools::install_github("ropensci/rdhs", ref = "issue33_path")
library(rdhs)

devtools::install_github("mrc-ide/demogsurv")
library(demogsurv)
library(data.table)
library(haven)
library(tidyverse)
library(plyr)
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
  dat <- dat[grep("caseid|^b8_|^b19_|^h11_|^h22|^h31_|^h33_", names(dat))]
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
data <- ir$NI2012DHS

c1 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b19_01, h33_1, h11_1, h22_1, h31_1)
c2 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b19_02, h33_2, h11_2, h22_2, h31_2)
c3 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b19_03, h33_3, h11_3, h22_3, h31_3)
c4 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b19_04, h33_4, h11_4, h22_4, h31_4)
c5 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b19_05, h33_5, h11_5, h22_5, h31_5)
c6 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b19_06, h33_6, h11_6, h22_6, h31_6)

#Older Recodes where age is recorded as years under variable b8
c1 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b8_01, h33_1, h11_1, h22_1, h31_1)
c2 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b8_02, h33_2, h11_2, h22_2, h31_2)
c3 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b8_03, h33_3, h11_3, h22_3, h31_3)
c4 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b8_04, h33_4, h11_4, h22_4, h31_4)
c5 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b8_05, h33_5, h11_5, h22_5, h31_5)
c6 <- data %>% select(SurveyId, CountryName, SurveyYear, caseid, b8_06, h33_6, h11_6, h22_6, h31_6)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','age','vas','dia','fever','cough')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','age','vas','dia','fever','cough')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','age','vas','dia','fever','cough')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','age','vas','dia','fever','cough')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','age','vas','dia','fever','cough')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','age','vas','dia','fever','cough')

data <- rbind(c1,c2,c3,c4,c5,c6)
data <-data %>% filter(age>5) %>% filter(age<60)
data <-data %>% filter(age>0) %>% filter(age<6)

data <- data %>% mutate(vasupp = case_when(vas=="vaccination date on card" ~ 1,
                                           vas=="reported by mother" ~ 1,
                                           vas=="vaccination marked on card" ~ 1,
                                           vas=="don't know" ~ 2,
                                           vas=="missing" ~ 2,
                                           vas=="no" ~ 2))

#Did most recently born child consume vitamin A-rich foods in the last 24H?
data <- data %>% mutate(inf = ifelse(dia=="yes, last 24 hours" |
                                     dia=="yes, last two weeks" |
                                     fever=="yes" |
                                     cough=="yes, last 24 hours" |
                                     cough=="yes, last two weeks", "yes", "no")) 

data %>% filter(!is.na(vasupp)) %>% filter(!is.na(inf)) %>% nrow()

table <- table(data$vasupp, data$inf)
table
prop.table(table, 2)
prop.test(1518,(1518+784),correct=FALSE)

