###########################################################
##### Data Cleaning Script - DMX Linkages
##### Shrimp biomass from NOAA / ADFG Small mesh trawl survey
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
# Shrimp biomass (units = ?) from NOAA / ADFG Small mesh trawl survey
# estimates sent to Stephai Zador from Dan Urban

URL_Shr <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ucFliX1NOZHRMOWc"
ShrGet <- GET(URL_Shr)
Shr1 <- content(ShrGet, as='text')
Shr_df <- read.csv(file=textConnection(Shr1),stringsAsFactors=FALSE)
head(Shr_df)
#
Shr_df <- Shr_df %>%
          rename(Year=year, Pink_Shrimp=p.shrimp)
#

