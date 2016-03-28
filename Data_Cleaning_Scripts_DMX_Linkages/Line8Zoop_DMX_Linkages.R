##################################################################################
##### Data Cleaning Script - DMX Linkages
##### Large zooplankton abundances from Line 8 (Shelikof Strait; EcoFOCI program)
##################################################################################

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

# these data are from the NOAA EcoFOCI sampling program conducted at Line 8 (southwest Shelikof Strait)

# units are indiv. / m^3
# data here were collected between April 25 and June 8
# pursuant to discussion with Janet Duffy-Anderson, Euphausiid and Mysid abundances were calculated using day and night collections (because nets sampled the entire water column beginning 10m above bottom)

# Please note:
# **only a subset of the zooplankton community was counted** - targeted species were known prey items of larval pollock
# data here are from the 333um mesh net only (153um mesh net are only available after 2002 and are not included here)
# therefore, smaller copeopd species are excluded (eg Oithona, Acartia, which are numerically dominant at Seward Line, as well as nauplii and early copepodite stages)


URL_zoop <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uMkk3V3JaQ3k2MTQ"
zoopGet <- GET(URL_zoop)
zoop1 <- content(zoopGet, as='text')
zoop_df <- read.csv(file=textConnection(zoop1),stringsAsFactors=FALSE)

Line8Zoop <- zoop_df %>%
  rename(Year = year) %>%
  mutate(Line8LargeCopepods = Ncristatus + Ebungii + Cmarshallae + Cpacificus + Mpacificalucens + Metridia + CalanidsUnid) %>%
  mutate(Line8Euphausiids = EuphausiidAdJuv + EuphausiidFurcillia) %>%
  mutate(Line8LargeOtherZooplankton = Chaetognatha + NatantiaAdJuv + InvertLarvae + Gammaridea + Hyperiidea + Siphonophora + Mysidacea) %>%
  select(Year, Line8LargeCopepods, Line8Euphausiids, Line8LargeOtherZooplankton)
head(Line8Zoop)