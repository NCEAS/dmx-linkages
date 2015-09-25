###########################################################
##### Data Cleaning Script - DMX Linkages
##### Pacific Cod (from NOAA stock assessments)
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
### Pacific Cod (from NOAA stock assessments):
# Metadata column header: female spawning biomass from Table 2.18 – Estimated female spawning biomass (t)
#                                               from the 2012 assessment and this year’s assessment
#                         Age 1 numbers in millins from Table Table 2.20 – Estimated numbers-at-age
#                                              (millions) at the time of spawning

URL_PC <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uNURmV3JvT1Y1eFU"
PC_Get <- GET(URL_PC)
PC1 <- content(PC_Get, as='text')
PC_df <- read.csv(file=textConnection(PC1),stringsAsFactors=FALSE,head=TRUE)
head(PC_df)
#
PC_df <- PC_df %>%
         rename(Year=year, PCod_female_Bmss_t=female.spawning.biomass,
                PCod_Age1_millions=Age.1.numbers..in.millions.)

#



