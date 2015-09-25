###########################################################
##### Data Cleaning Script - DMX Linkages
##### Pollock Biomass (from NOAA stock assessments)
###########################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)

## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############

### Pollock Biomass (from NOAA stock assessments):

URL_T19 <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uSFJYVXE0ZGNLaHc"
PollGet <- GET(URL_T19)
Poll1 <- content(PollGet, as='text')
Poll_df <- read.csv(file=textConnection(Poll1),stringsAsFactors=FALSE)
head(Poll_df)
#
Poll_Adult <- Poll_df %>%
              rename(Poll_Yr3plus_TtlBmss_1000Tons=X3..total.biomass...1.000.t.,
                     Catch_tons=Catch..t.) %>%
              filter(!is.na(Year)) %>%
              select(Year, Poll_Yr3plus_TtlBmss_1000Tons)

Poll_Yr1 <- Poll_df %>%
            rename(Poll_Age1_recruits_millions=Age.1.recruits..million.,
                   Catch_tons=Catch..t.) %>%
            filter(!is.na(Year)) %>%
            select(Year, Poll_Age1_recruits_millions)

#


