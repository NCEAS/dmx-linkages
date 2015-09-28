###########################################################
##### Data Cleaning Script - DMX Linkages
##### Pollock Fishery Data
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
# Pollock Fishery Data

URL_PollFishery <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ual9URUl0THRBUGs"
PollFishery_Get <- GET(URL_PollFishery)
PollFishery1 <- content(PollFishery_Get, as='text')
PollFishery_df <- read.csv(file=textConnection(PollFishery1),stringsAsFactors=FALSE,head=TRUE)

PollFishery_df <- PollFishery_df %>%
                  rename(Year=year)
#

