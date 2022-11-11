
# Example rdhs file

# Use this version, because there are a few errors that have not been resolved in the cran version yet
devtools::install_github("ropensci/rdhs", ref = "issue33_path")
library(rdhs)

devtools::install_github("mrc-ide/demogsurv")
library(demogsurv)
library(data.table)
library(haven)
library(tidyverse)
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
cc <- c( "ET",  "MW")

# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=1990, surveyType = "DHS")

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
  dat <- dat[grep("caseid|^v0|^v1|^b", names(dat))]
  ir[[survid]] <- dat
}

ir <- lapply(ir, haven::as_factor)

## Add survey-level variables
ir <- Map(data.frame,
          SurveyId = surveys$SurveyId,
          CountryName = surveys$CountryName,
          SurveyYear = surveys$SurveyYear,
          ir)

# adjust ethiopian calendar for the CMC columns
ir$ET2000DHS[, grep("b3_", colnames(ir$ET2000DHS))] <- ir$ET2000DHS[, grep("b3_", colnames(ir$ET2000DHS))] + 92
ir$ET2005DHS[, grep("b3_", colnames(ir$ET2005DHS))] <- ir$ET2005DHS[, grep("b3_", colnames(ir$ET2005DHS))] + 92
ir$ET2010DHS[, grep("b3_", colnames(ir$ET2010DHS))] <- ir$ET2010DHS[, grep("b3_", colnames(ir$ET2010DHS))] + 92
ir$ET2011DHS[, grep("b3_", colnames(ir$ET2011DHS))] <- ir$ET2011DHS[, grep("b3_", colnames(ir$ET2011DHS))] + 92
ir$ET2000DHS$v008 <- ir$ET2000DHS$v008 + (92)
ir$ET2005DHS$v008 <- ir$ET2005DHS$v008 + (92)
ir$ET2011DHS$v008 <- ir$ET2011DHS$v008 + (92)
ir$ET2016DHS$v008 <- ir$ET2016DHS$v008 + (92)
ir$ET2000DHS$v011 <- ir$ET2000DHS$v011 + (92)
ir$ET2005DHS$v011 <- ir$ET2005DHS$v011 + (92)
ir$ET2011DHS$v011 <- ir$ET2011DHS$v011 + (92)
ir$ET2016DHS$v011 <- ir$ET2016DHS$v011 + (92)
ir$ET2000DHS$v017 <- ir$ET2000DHS$v017 + (92)
ir$ET2005DHS$v017 <- ir$ET2005DHS$v017 + (92)
ir$ET2011DHS$v017 <- ir$ET2011DHS$v017 + (92)
ir$ET2016DHS$v017 <- ir$ET2016DHS$v017 + (92)

ir <- lapply(ir, function(x){ x$SurveyId <- factor(x$SurveyId) ; return(x)})
ir <- lapply(ir, function(x){ x$CountryName <- factor(x$CountryName) ; return(x)})
ir <- lapply(ir, function(x){ x$SurveyYear <- factor(x$SurveyYear) ; return(x)})

# Calculate total fertility rate for each survey in the list

tfr <- lapply(ir, calc_tfr, by=~SurveyId+CountryName+SurveyYear, tips = c(0,15), period=1990:2016 ,strata=NULL)

df.tfr <- do.call(rbind, tfr)
df.tfr$period <- as.numeric(levels(df.tfr$period))[df.tfr$period]
df.tfr$SurveyYear <- as.numeric(levels(df.tfr$SurveyYear))[df.tfr$SurveyYear]
df.tfr$YrBeforeSurvey <- df.tfr$SurveyYear - df.tfr$period
df.tfr$cutoff <- df.tfr$SurveyYear - 5
# remove data that is from -1 years before survey
# these are years that snuck in somehow. data for years after the date of the survey
df.tfr <- subset(df.tfr, !(YrBeforeSurvey < 0))

ggplot() +
  geom_line(data = df.tfr, aes(x = period, y = tfr, col = SurveyYear, group = SurveyYear)) +
  geom_point(data = subset(df.tfr, SurveyYear==period), aes(x = SurveyYear, y = tfr)) +
  geom_vline(data = df.tfr, aes(xintercept = df.tfr$cutoff), linetype = "dashed", alpha = .5) +
  facet_wrap(~CountryName) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlim(1990,2015)

