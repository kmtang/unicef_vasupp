
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
cc <- c( "ET","MW","CM","BJ","ML","NP","PK","ZA")
cc <- c("AF","AO","TD","BU","CG","CD","CI","GA","KH","BO")
cc <- c("BD","GM","GH","KM","SZ","GN","HT","KE","LS","LB")
cc <- c( "MZ","MM","NM","NG","PG","PH","RW","ST","SN","SL","TJ","TZ","TL","TG","UG","YE","ZM","ZW")

cc <- c("BF","MD","NI","AF",)

# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=2005, surveyType = "DHS")

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
  dat <- dat[grep("caseid|v001|v005|^h33_|^b8_|^b19_|^v102|v191", names(dat))]
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
data <- ir$CM2018DHS
#Reformat survey weights
data$v005 <- data$v005/1000000 
data$v191 <- data$v191/100000
data$v191a <- data$v191a/100000

#Rural SEP
data.rural <- data %>% filter(v102=="rural")
data.rural$res.sep <- ntile(data.rural$v191a, 3)
#Urban SEP
data.urban <- data %>% filter(v102=="urban")
data.urban$res.sep <- ntile(data.urban$v191a, 3)
data <- rbind(data.rural,data.urban)

#Newer data (post 2012)
c1 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_01, b19_01, h33_1, v102, res.sep)
c2 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_02, b19_02, h33_2, v102, res.sep)
c3 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_03, b19_03, h33_3, v102, res.sep)
c4 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_04, b19_04, h33_4, v102, res.sep)
c5 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_05, b19_05, h33_5, v102, res.sep)
c6 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_06, b19_06, h33_6, v102, res.sep)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'age_m','vas','res', 'res.sep')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'age_m','vas','res', 'res.sep')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'age_m','vas','res', 'res.sep')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'age_m','vas','res', 'res.sep')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'age_m','vas','res', 'res.sep')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'age_m','vas','res', 'res.sep')

data <- rbind(c1,c2,c3,c4,c5,c6)
data <-data %>% filter(age_m>5) %>% filter(age_m<60)

#Older Data (pre 2012)
c1 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_01, h33_1, v102, res.sep)
c2 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_02, h33_2, v102, res.sep)
c3 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_03, h33_3, v102, res.sep)
c4 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_04, h33_4, v102, res.sep)
c5 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_05, h33_5, v102, res.sep)
c6 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, b8_06, h33_6, v102, res.sep)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'vas','res', 'res.sep')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'vas','res', 'res.sep')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'vas','res', 'res.sep')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'vas','res', 'res.sep')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'vas','res', 'res.sep')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight','age', 'vas','res', 'res.sep')

data <- rbind(c1,c2,c3,c4,c5,c6)
data <-data %>% filter(age>0) %>% filter(age<6)

#Recategorise outcome variable
data <- data %>% mutate(vasupp = case_when(vas=="vaccination date on card" ~ 1,
                                           vas=="reported by mother" ~ 1,
                                           vas=="vaccination marked on card" ~ 1,
                                           vas=="no" ~ 2))

data <- data %>% mutate(res.sep = as.factor(case_when(res.sep=="1" ~ "1. Poor",
                                                      res.sep=="2" ~ "2. Middle",
                                                      res.sep=="3" ~ "3. Wealthy")))

#Adjust for DHS weights
w_data <- data %>% as_survey_design(id = cluster, strata =NULL, weights = weight, nest=T)

#Calculate stratified coverage and 95%CI limits 
CM <- w_data %>%
  srvyr::group_by(res.sep,res) %>%
  srvyr::summarise(
    vasupp = (survey_mean(vasupp == 1, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) 

CM$country <- "Cameroon"

vas_cov <- rbind(BJ,CM)

#Plot
gplot_f <- ggplot(vas_cov) + 
  geom_pointrange(aes(x =country, y = vasupp, ymin = vasupp_low, ymax = vasupp_upp, color = res, shape=res.sep))+ 
  theme_bw() +
  coord_flip(ylim = c(0, 100)) +
  labs(y = "Percentage of children consuming VAS in the prior 6 months",
       x = "Country") +
  scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1))+
  theme(plot.caption = element_text(hjust = 1))

#Create order for dumbell plot
order <- vas_cov %>% filter(pne_cat=="yes") %>% arrange(vasupp) %>% select(country) %>% dplyr::mutate(order = row_number())
vas_cov<- merge(x=vas_cov, y=order, by.x='country', by.y='country', fill=-9999, all.x = TRUE)


write.csv(vas_cov, "/Users/kevintang/Library/Mobile Documents/com~apple~CloudDocs/Documents/r_projects/unicef_vasupp/data/vas_pne.csv", row.names=FALSE)
