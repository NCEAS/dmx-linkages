###########################################################
##### Data Cleaning Script - DMX Linkages
##### Arrowtooth adult biomass (from 2013 SAFE Stock Assessment)
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
### Arrowtooth adult biomass (age-3 plus; tons)
#
URL_Arr <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uSVh6Rl9FNXFwRm8"
ArrGet <- GET(URL_Arr)
Arr1 <- content(ArrGet, as='text')
Arr_df <- read.csv(file=textConnection(Arr1),stringsAsFactors=FALSE)

#