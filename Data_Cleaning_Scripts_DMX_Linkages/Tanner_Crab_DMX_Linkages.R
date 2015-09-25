###########################################################
##### Data Cleaning Script - DMX Linkages
##### Tanner Crab Abundances
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

# Tanner Crab
# Data are from Spalinger 2015 (ADFG Fishery Management Report No. 15-27):
# Cite as: Spalinger, K. 2015. Bottom trawl survey of crab and groundfish: Kodiak, Chignik,
         # South Peninsula, and Eastern Aleutian Management Districts, 2014. Alaska Department of
         # Fish and Game, Fishery Management Report No. 15-27, Anchorage.
# Data are abundances pulled from Tables 3 & 7
URL_TCrab <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7udDlyLTU4dDlwa0U"
TCrab_Get <- GET(URL_TCrab)
TCrab1 <- content(TCrab_Get, as='text')
TCrab_df <- read.csv(file=textConnection(TCrab1),stringsAsFactors=FALSE,head=TRUE)
head(TCrab_df)
#
TCrab <- TCrab_df %>%
         filter(Year != 1988) %>% # some sites not sampled in 1988
         filter(Year != 1995) %>% # some sites not sample in 1995
         mutate(TotTCrab = KodiakTotalTannerCrabAbund + WesternSectionTotals +
                EasternSectionTotals + SouthPeninsulaDistrictTotals + ChignikDistrictTotals) %>%
         select(Year, TotTCrab)



