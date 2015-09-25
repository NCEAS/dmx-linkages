###########################################################
##### Data Cleaning Script - DMX Linkages
##### Water Temperature (SST): from Seward Line CTD Data
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
###  Water Temperature (SST):

URL_T <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.31.1"
TGet <- GET(URL_T)
T1 <- content(TGet, as='text')
Tmps <- read.csv(file=textConnection(T1),stringsAsFactors=FALSE,strip.white=TRUE)
head(Tmps)

URL_Ts <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.32.2"
TsGet <- GET(URL_Ts)
Ts1 <- content(TsGet, as='text')
TmpSams <- read.csv(file=textConnection(Ts1),stringsAsFactors=FALSE,strip.white=TRUE)
head(TmpSams)
#
Temps <- merge(Tmps, TmpSams, all.x=TRUE)  # merge sample information with data values
Temps$Date <- sapply((strsplit(as.character(Temps$dateTime), split=" ")), function(x) x[1]) # split date out
head(Temps)

############################################
### NOTE : Need to deal with missing sample info for cruiseID TXS09, consecStationNum 3
############################################
# missing_date <- filter(Temps, is.na(dateTime))  # selects data with missing dates
# miss_cID <- unique(missing_date$cruiseID) # selects the cruise IDs for which sample info is missing


SST <- Temps %>%
             mutate(Year=sapply((strsplit(as.character(Date), split="/")),
                                function(x) x[3])) %>%   # creates Year column
             arrange(dateTime) %>%
             rename(WTemp_C=temp) %>%
             group_by(Year) %>%
             summarise(WTemp_C_AnnMn=mean(WTemp_C)) %>% # get annual means
             ungroup() %>%
             select(Year, WTemp_C_AnnMn)  # selects columns wanted
#


