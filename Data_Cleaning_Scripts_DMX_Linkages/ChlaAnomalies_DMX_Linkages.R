###########################################################
##### Data Cleaning Script - DMX Linkages
##### Mean annual Chl a anomalies (mg/m3) for Gulf of Alaska
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
# Mean annual Chl a anomalies (mg/m3) for Gulf of Alaska
# From Waite & Mueter 2013, Fig 11 Annual
# Waite, J.N. and Mueter, F.J. 2013. Spatial and temporal variability of chlorophyll-a concentrations
# in the coastal Gulf of Alaska, 1998-2011, using cloud-free reconstructions of SeaWiFS and MODIS-Aqua data.
# Prog. Oceanogr. 116, 179-192.

URL_Chl <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uRHdOTGQtSVBQOE0"
ChlGet <- GET(URL_Chl)
Chl1 <- content(ChlGet, as='text')
Chl_df <- read.csv(file=textConnection(Chl1),stringsAsFactors=FALSE)

#