###########################################################
##### Data Cleaning Script - DMX Linkages
##### Capelin Index from Steph Zador
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
### Capelin Index
# derived index based on 6 different capelin sampling methods (occurrence of capelin in diets of 4 groudfish sp and 2 seabird sp)

URL_Cap <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uaC1Ta0VyeW9oX0k"
CapGet <- GET(URL_Cap)
Cap1 <- content(CapGet, as='text')
Cap_df <- read.csv(file=textConnection(Cap1),stringsAsFactors=FALSE)