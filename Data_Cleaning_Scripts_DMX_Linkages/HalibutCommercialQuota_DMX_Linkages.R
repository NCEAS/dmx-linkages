###########################################################
##### Data Cleaning Script - DMX Linkages
##### Halibut Commercial Quota
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
### Halibut Commercial Catch Limits
### bottom half of Table 5 in the 2015 RARA Halibut stock assessement (Commercial Fishery chapter)
### units = thousands of pounds, net weight (75% of round weight)

URL_HlbtQ <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ucG11QzNrbkpyVGs"
HlbtQGet <- GET(URL_HlbtQ)
HlbtQ1 <- content(HlbtQGet, as='text')
HlbtQuota_df <- read.csv(file=textConnection(HlbtQ1),stringsAsFactors=FALSE)

HlbtQuota_df1 <- HlbtQuota_df %>% 
  dplyr::rename(Year=Area) %>%
  rowwise() %>%
  mutate(HlbtQuota_OurAreas = X2C + X3A +X3B) %>% # sum quota for areas 2C, 3A, 3B
  ungroup() %>%
  mutate(HlbtQuota_2C3A3B_RoundWt_ThouLbs = HlbtQuota_OurAreas/0.75) %>% # convert to total round weight
  mutate(HlbtQuota_2C3A3B = HlbtQuota_2C3A3B_RoundWt_ThouLbs*1000) %>% # scale from (thousands of lbs) to (lbs)
  select(Year, HlbtQuota_2C3A3B)

# units for final cleaned data are lbs Round Weight (ie 100 %)