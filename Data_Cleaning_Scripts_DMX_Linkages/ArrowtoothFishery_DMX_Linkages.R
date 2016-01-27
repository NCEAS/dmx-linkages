###########################################################
##### Data Cleaning Script - DMX Linkages
##### Arrowtooth fishery data (from Steve Kasperski Jan 26 2016)
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
### Arrowtooth fishery data
#
URL_ArrFishery <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uTVh2ZVJNTUlLeVE"
ArrFisheryGet <- GET(URL_ArrFishery)
ArrFishery1 <- content(ArrFisheryGet, as='text')
ArrFishery_df <- read.csv(file=textConnection(ArrFishery1),stringsAsFactors=FALSE)

ArrFishery_df <- ArrFishery_df %>% rename(Year=year)
