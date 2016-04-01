###########################################################
##### Data Cleaning Script - DMX Linkages
##### North Pacific Index (NPI) for sea level pressure
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


### North Pacific Index (NPI) for sea level pressure:
URL_npi <- "https://climatedataguide.ucar.edu/sites/default/files/climate_index_files/npindex_monthly.ascii"
npiGet <- GET(URL_npi)
npi1 <- content(npiGet, as='text')
npi <- read.table(file=textConnection(npi1),stringsAsFactors=F, sep=" ", header=TRUE, fill=TRUE)
npi[1:50,]
#
NPI <- npi %>%
           rename(YearMon=X, SeaLevelPressure_hPa=and) %>% # rename columns with data
           select(YearMon, SeaLevelPressure_hPa) %>% # remove columns without data
           mutate(Year=substring(YearMon,1,4),   # creates Year column
                  Month=substring(YearMon,5,6)) %>%  # creates Month column
           filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
           filter(!is.na(SeaLevelPressure_hPa),
                  SeaLevelPressure_hPa != -999.00) %>% # remove NA values, and -999 values which are NAs
           filter(Month %in% c("01","02","03","11","12")) %>%
           group_by(Year) %>%
           summarise(SeaLevelPressure_Winter_hPa=mean(SeaLevelPressure_hPa)) %>%  # winter means
           #summarise(SeaLevelPressure_mean_hPa=mean(SeaLevelPressure_hPa)) %>% # get annual means
           ungroup()
#


# Winter is Nov thru March






