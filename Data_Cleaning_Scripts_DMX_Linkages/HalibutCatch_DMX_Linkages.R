###########################################################
##### Data Cleaning Script - DMX Linkages
##### Halibut Fishery Data
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
# Halibut Fishery Data

URL_HlbtFishery <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uY0JVVHQxSWxXYVU"
HlbtFishery_Get <- GET(URL_HlbtFishery)
HlbtFishery1 <- content(HlbtFishery_Get, as='text')
HlbtFishery_df <- read.csv(file=textConnection(HlbtFishery1),stringsAsFactors=FALSE,head=TRUE)

HlbtFishery_df <- HlbtFishery_df %>%
                  rename(Year=year)
#