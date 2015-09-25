###########################################################
##### Data Cleaning Script - DMX Linkages
##### Sharks & Skates from NOAA 
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
# Sharks & Skates
# Skates = total skate BIOMASS (tons) from GoA Large Mesh Trawl surveys
# Table 1 in 2013 AFSC Skate Stock Assessment Report (available from http://www.afsc.noaa.gov/refm/stocks/2013_assessments.htm)
#
# Sharks = total shark NUMBERS from IPHC longline survey
# Table 20.A.1 in 2011 AFSC Shark Stock Assessment Report (available from http://www.afsc.noaa.gov/refm/stocks/2011_assessments.htm)
#
URL_ShSk <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uc1BiN3lUVERySzQ"
ShSk_Get <- GET(URL_ShSk)
ShSk1 <- content(ShSk_Get, as='text')
ShSk_df <- read.csv(file=textConnection(ShSk1),stringsAsFactors=FALSE,head=TRUE)

#


