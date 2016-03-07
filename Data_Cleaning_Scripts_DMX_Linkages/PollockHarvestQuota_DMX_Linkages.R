###########################################################
##### Data Cleaning Script - DMX Linkages
##### Pollock Harvest Quota (Total Allowable Catch)
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

# Data are from Table 1.1 in the 2014 Pollock Stock Assessment
# Units = tons

URL_PollTAC <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uSjc4U0pZTVFEQmM"
PollTACGet <- GET(URL_PollTAC)
PollTAC1 <- content(PollTACGet, as='text')
PollTAC_df <- read.csv(file=textConnection(PollTAC1),stringsAsFactors=FALSE)
head(PollTAC_df)


