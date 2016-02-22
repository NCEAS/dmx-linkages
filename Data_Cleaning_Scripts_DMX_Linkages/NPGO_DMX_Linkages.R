###########################################################
##### Data Cleaning Script - DMX Linkages
##### North Pacific Gyre Oscillation Index (NPGO)
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

#############    xpathSApply(xmlParse(content(GET(URL)))
# North Pacific Gyre Oscillation Index (NPGO):
URL_npgo <- "http://www.o3d.org/npgo/npgo.php"
npgo_pre <- xpathSApply(xmlParse(content(GET(URL_npgo))),"/html/body/pre", xmlValue)
npgo_cols <- scan(textConnection(npgo_pre), skip=25, nlines=1, what=character())# Get header row
npgo_cols <- npgo_cols[2:4] # select column names
npgo_df <- read.csv(file=textConnection(npgo_pre), skip=26, stringsAsFactors=F, sep="",
                 header=FALSE, col.names=npgo_cols, strip.white=TRUE)

npgo_annual <- npgo_df %>%
               rename(Year=YEAR) %>% # rename data columns
               filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
               group_by(Year) %>%
               summarise(NPGO_anul_mn=mean(NPGO)) %>% # get annual means
               ungroup()  #
#


