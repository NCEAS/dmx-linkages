###########################################################
##### Data Cleaning Script - DMX Linkages
##### Multivariate El Nino Southern Oscillation Index (MEI):
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
###  Multivariate ENSO Index (MEI):
URL_enso <- "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
enso_pre <- xpathSApply(xmlParse(content(GET(URL_enso))),"/html/body/pre", xmlValue)
enso_cols <- scan(textConnection(enso_pre), skip=10, nlines=1, what=character()) # get header row
enso <- read.csv(file=textConnection(enso_pre), skip=11, stringsAsFactors=F, sep="\t",
                 header=FALSE, col.names=enso_cols)
enso_df <- enso[1:66,]  # removes the text at bottom of file
#
ENSO_annual <- enso_df %>%
                    rename(Year=YEAR) %>% # rename data columns
                    filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
                    gather(Months, ENSO, -Year) %>% # reshapes data to be column-wise
                    filter(!is.na(ENSO)) %>% # remove NA values
                    group_by(Year) %>%
                    summarise(ENSO_anul_mn=mean(ENSO)) %>% # get annual means
                    ungroup()  #
#
# This was my initial try, which didn't work:
# The readHTMLTable() help page provides an example of reading a plain text table out of an
# HTML <pre> element using htmlParse(), getNodeSet(), textConnection() and read.table()





