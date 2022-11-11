
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
cc <- c( "ET","MW","CM","BF","BJ","CF","MD","ML","NP","NI","PK","ZA")
cc <- c("AF","AO","TD","BU","CG","CD","CI","GA","KH","BO")
cc <- c("BD","GM","GH","KM","SZ","GN","HT","KE","LS","LB")
cc <- c( "MZ","MM","NM","NG","PG","PH","RW","ST","SN","SL","TJ","TZ","TL","TG","UG","YE","ZM","ZW")

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
  dat <- dat[grep("caseid|v001|v005|h33_1|b8_01|b19_01|v414g|v414h|v414i|v414j|v414k|v414l|v414m|v414n", names(dat))]
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

#Country by country analysis
data <- ir$PG2017DHS

#Reformat survey weights
data$v005 <- data$v005/1000000 

data <- data %>% filter(b19_01>5) %>% filter(b19_01<24)
data <-data %>% filter(b8_01<2)

#Recategorise outcome variable
data <- data %>% mutate(vasupp = case_when(h33_1=="vaccination date on card" ~ 1,
                                           h33_1=="reported by mother" ~ 1,
                                           h33_1=="vaccination marked on card" ~ 1,
                                           h33_1=="don't know" ~ 2,
                                           h33_1=="missing" ~ 2,
                                           h33_1=="no" ~ 2))

#Did most recently born child consume vitamin A-rich foods in the last 24H?
data <- data %>% mutate(vafood = ifelse(v414g=="yes" |
                                   v414h=="yes" |
                                   v414i=="yes" |
                                   v414j=="yes" |
                                   v414k=="yes" |
                                   v414l=="yes" |
                                   v414m=="yes" |
                                   v414n=="yes", "yes", "no")) 

#Adjust for DHS weights
w_data <- data %>% as_survey_design(id = v001, strata =NULL, weights = v005, nest=T)

#Calculate stratified coverage and 95%CI limits 
PG <- w_data %>%
  srvyr::group_by(vafood) %>%
  srvyr::summarise(
    vasupp = (survey_mean(vasupp == 1, proportion = TRUE, vartype = "ci",na.rm = T)) * 100) %>% 
  filter(!is.na(vafood))

PG$country <- "Papua New Guinea"

vas_cov <- rbind(AF,AO,BD,BO,BJ,BF,BU,KH,KM,CG,CM,TD,CD,CI,SZ,ET,GA,GH,GM,GN,HT,KE,LS,LB,MD,MW,ML,MZ,MM,NM,NI,NP,NG,PK,PG,RW,ST,SN,SL,TJ,TZ,TL,TG,UG,YE,ZA,ZM,ZW)
#No dietary data from Philippines

#Create order for dumbell plot
order <- vas_cov %>% filter(vafood=="yes") %>% arrange(vasupp) %>% select(country) %>% dplyr::mutate(order = row_number())
vas_cov<- merge(x=vas_cov, y=order, by.x='country', by.y='country', fill=-9999, all.x = TRUE)

#Plot
gplot_a <- ggplot(vas_cov) + 
  geom_pointrange(aes(x = reorder(country, order), y = vasupp, ymin = vasupp_low, ymax = vasupp_upp, color = vafood))+ 
  theme_bw() +
  coord_flip(ylim = c(0, 100)) +
  labs(y = "Percentage of children consuming VAS in the prior 6 months",
       x = "Country",
       caption = "*Philippines did not collect dietary recall data in DHS individual recode") +
  scale_y_continuous(breaks = round(seq(min(0), max(100), by = 20),1))+
  theme(plot.caption = element_text(hjust = 1))

write.csv(vas_cov, "/Users/kevintang/Library/Mobile Documents/com~apple~CloudDocs/Documents/r_projects/unicef_vasupp/data/vas_diet.csv", row.names=FALSE)
