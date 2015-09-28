###########################################################
##### Data Cleaning Script - DMX Linkages
##### Zooplankton (from Seward Line dataset)
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
### Zooplankton (from Seward Line dataset)
# for full zooplankton processing scripts see dmx-common repository
# Caution: Use as a placeholder for now: Dataset needs some QC, and gear change in 2005 not yet accounted for
#
# Load output of May Small zooplankton processing script. Values are biomass (g WW / m3):
URL_SZo <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uUXhnd2c3REszVDQ"
SZoGet <- GET(URL_SZo)
SZo1 <- content(SZoGet, as='text')
SZo_df <- read.csv(file=textConnection(SZo1),stringsAsFactors=FALSE)
head(SZo_df)
#
# Load output of May Large zooplankton processing script. Values are biomass (g WW / m3):
URL_LZo <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uSHRtNWdCQVRUWUE"
LZoGet <- GET(URL_LZo)
LZo1 <- content(LZoGet, as='text')
LZo_df <- read.csv(file=textConnection(LZo1),stringsAsFactors=FALSE)
head(LZo_df)
#
MayZo = full_join(SZo_df, LZo_df, by = "Year") # merge Small & Large Zooplankton datasets
#
MayCopepods <- MayZo %>%
               mutate(MayCopepods = rowSums(MayZo[,2:35], na.rm=T)) %>% # sum all small & large copepods
               select(Year, MayCopepods)
#
MayEuphausiids <- MayZo %>%
                  select(Year, Euphausiids)
#


