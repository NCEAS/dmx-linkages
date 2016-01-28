###########################################################
##### Data Cleaning Script - DMX Linkages
##### Mean annual Chl a (mg/m3) for Gulf of Alaska
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

# Data from Waite, J.N. and Mueter, F.J. 2013. Spatial and temporal variability of chlorophyll-a concentrations
# in the coastal Gulf of Alaska, 1998-2011, using cloud-free reconstructions of SeaWiFS and MODIS-Aqua data.
# Prog. Oceanogr. 116, 179-192.
# Data sent by Jason Waite Sept 25 2015

# data are log mean annual Chl a (mg/m3), calculated from 8-day mean log chl-a levels for weeks 7-38
# regions: se = southeast, woff = western offshelf, won = western onshelf, cen = central
# NB this processing script is in dmx-common repository

URL_Chl <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uMjNnWnZGMnBoUFk"
ChlGet <- GET(URL_Chl)
Chl1 <- content(ChlGet, as='text')
Chl_df <- read.csv(file=textConnection(Chl1),stringsAsFactors=FALSE)

# calculate means across western offshelf, western onshelf, and central regions:
AnnChl = Chl_df %>%
  rename(Year=year) %>%
  mutate(AnnChl = rowMeans(Chl_df[,2:4])) %>%
  select(Year, AnnChl)
#View(AnnChl)