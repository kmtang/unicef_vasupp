
# Example rdhs file

# Use this version, because there are a few errors that have not been resolved in the cran version yet
devtools::install_github("ropensci/rdhs", ref = "issue33_path")
library(rdhs)
library(demogsurv)
library(data.table)
pacman::p_load(rio,haven,survey,srvyr,gtsummary,tidyverse)
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
cc <- c( "ET","MW","CM","BF","BJ","CF","MD","ML","NP","NI","PK","ZA","AF","TD","BU","CG","CD","CI","GA","KH","BO","GM","GH","KM","SZ","GN","HT","KE","LS","LB")
cc <- c( "MZ","MM","NM","NG","PG","PH","RW","ST","SN","SL","TJ","TZ","TL","TG","YE","ZM","ZW")
cc <- c( "AO","BD")
cc <- c( "IA")
# Use rdhs to retreive datasets, downloading them from DHS website if not already in the rdhs cache.
surveys <- dhs_surveys(countryIds = cc, surveyYearStart=2010, surveyType = "DHS")

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
  dat <- dat[grep("caseid|v001|v005|^h33_|^b8_0|^b19_0|v106|v107", names(dat))]
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

#var<- ir$MW2015DHS
#Vitamin A
data <- ir$BD2017DHS

#Adjust for survey weights
data$v005 <- data$v005/1000000 

#Newer data (post 2012)
c1 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_01, b19_01, h33_1)
c2 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_02, b19_02, h33_2)
c3 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_03, b19_03, h33_3)
c4 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_04, b19_04, h33_4)
c5 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_05, b19_05, h33_5)
c6 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_06, b19_06, h33_6)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'age_m','vas')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'age_m','vas')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'age_m','vas')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'age_m','vas')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'age_m','vas')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'age_m','vas')

#Older Data (pre 2012)
c1 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_01, h33_1)
c2 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_02, h33_2)
c3 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_03, h33_3)
c4 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_04, h33_4)
c5 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_05, h33_5)
c6 <- data %>% dplyr::select(SurveyId, CountryName, SurveyYear, caseid, v001, v005, v106, b8_06, h33_6)

names(c1) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'vas')
names(c2) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'vas')
names(c3) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'vas')
names(c4) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'vas')
names(c5) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'vas')
names(c6) <- c('SurveyId','CountryName','SurveyYear','caseid','cluster', 'weight', 'matedu', 'age', 'vas')

data <- rbind(c1,c2,c3,c4,c5,c6)

#Filter children in 6-59 month age range
data <-data %>% filter(age_m>5) %>% filter(age_m<60)
data <-data %>% filter(age>0) %>% filter(age<6)

data <- data %>% mutate(vasupp = case_when(vas=="vaccination date on card" ~ 1,
                                           vas=="reported by mother" ~ 1,
                                           vas=="vaccination marked on card" ~ 1,
                                           vas=="don't know" ~ 2,
                                           vas=="missing" ~ 2,
                                           vas=="no" ~ 2))

data <- data %>% mutate(matedu2 = case_when(matedu=="no education" ~ "no education",
                                            matedu=="primary" ~ "primary",
                                            matedu=="secondary" ~ "secondary+",
                                            matedu=="higher" ~ "secondary+"))

w_data <- data %>% as_survey_design(id = cluster, strata =NULL, weights = weight, nest=T)

BD <- w_data %>%
  srvyr::group_by(matedu2) %>%
  srvyr::summarise(
    vasupp = (survey_mean(vasupp == 1, proportion = TRUE, vartype = "ci",na.rm = T)) * 100)

BD$country <- "Bangladesh"
#KM <- KM %>% filter(!is.na(matedu2))

vas_cov <- rbind(AF,AO,BD,BO,BJ,BF,BU,CM,TD,CG,CD,CI,KH,KM,ET,SZ,GA,GM,GH,GN,HT,KE,LS,LB,ML,MD,MW,MZ,MM,NI,NM,NG,NP,PG,PH,PK,RW,ST,SN,SL,TJ,TZ,TL,TG,ZA,ZM,ZW)
#No womens education data from Yemen

#Create order for dumbbell plot
order <- vas_cov %>% filter(matedu2=="secondary+") %>% arrange(vasupp) %>% select(country) %>% dplyr::mutate(order = row_number())
vas_cov<- merge(x=vas_cov, y=order, by.x='country', by.y='country', fill=-9999, all.x = TRUE)

#Plot
ggplot(vas_cov) + 
  geom_pointrange(aes(x = reorder(country, order), y = vasupp, ymin = vasupp_low, ymax = vasupp_upp, color = matedu2))+ 
  theme_bw() +
  coord_flip(ylim = c(0, 100)) +
  labs(y = "Percentage of children consuming VAS in the prior 6 months",
       x = "Country",
       caption = "*Yemen did not collect maternal educational attainment data in DHS individual recode") +
  scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1))+
  theme(plot.caption = element_text(hjust = 1))

write.csv(vas_cov, "/Users/kevintang/Library/Mobile Documents/com~apple~CloudDocs/Documents/r_projects/unicef_vasupp/data/vas_matedu.csv", row.names=FALSE)
vas_cov <- read.csv("/Users/kevintang/Library/Mobile Documents/com~apple~CloudDocs/Documents/r_projects/unicef_vasupp/data/vas_diet.csv")

gplot_a #diet
gplot_b #matedu
gplot_c #measles
gplot_d #dtp3
gplot_e #rota
gplot_f #pne
