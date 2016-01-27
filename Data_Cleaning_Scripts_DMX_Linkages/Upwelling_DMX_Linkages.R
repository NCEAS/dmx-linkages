###########################################################
##### Data Cleaning Script - DMX Linkages
##### Upwelling Anomalies North Pacific Ocean
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

### Upwelling Anomalies:
URL_upanom <- "http://www.pfeg.noaa.gov/products/PFELData/upwell/monthly/upanoms.mon"
upanom_raw <- read_html(URL_upanom)
upanom_pre <- upanom_raw %>%
              html_node("p") %>%
              html_text()
upanom_cols <- scan(textConnection(upanom_pre), skip=2, nlines=1, what=character())# Get header row
upanom_cols <- c("Lat", "Long", upanom_cols[-1])# split position into lat and long
upanom_df <- read.csv(file=textConnection(upanom_pre), skip=4, stringsAsFactors=F, sep="",
                   header=FALSE, col.names=upanom_cols, strip.white=TRUE)
#
upanom <- upanom_df %>%
          rename(Year=YEAR) %>% # rename data columns
          filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
          gather(Month, UpwelAnom,-Year,-Lat,-Long) %>% # reshapes data to be column-wise
          group_by(Year) %>%
          summarise(UpWelAnom_anul_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
          ungroup()

#



