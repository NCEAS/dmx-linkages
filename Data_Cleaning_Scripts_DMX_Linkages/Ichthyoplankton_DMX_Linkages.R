###########################################################
##### Data Cleaning Script - DMX Linkages
##### Ichthyoplankton Data (Arrowtooth, Pollock, Halibut)
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
# Data are from Janet Duffy-Anderson, from the EcoFOCI sampling program in & southwest of Shelikof Strait
URL_Ich <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ualZkTUsyemluYzg"
IchGet <- GET(URL_Ich)
Ich1 <- content(IchGet, as='text')
Ich_df <- read.csv(file=textConnection(Ich1),stringsAsFactors=FALSE)