###########################################################
##### Data Cleaning Script - DMX Linkages
##### Fish data from Sarah Gaichas
#####*****NB Sarah cautions that these data should be used only as placeholders 
#####     until they're updated

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
### Fish data from Sarah Gaichas
# NB Sarah cautions that these data should be used only as placeholders until they're updated
#
URL_Fish <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ubGJLYXQwRlR0Ujg"
FishGet <- GET(URL_Fish)
Fish1 <- content(FishGet, as='text')
Fish_df <- read.csv(file=textConnection(Fish1),stringsAsFactors=FALSE)
#head(Fish_df)
#names(Fish_df)
#
# extract Capelin biomass
# units are Biomass (tons/km2) from surveys
CapelinBiomass <- Fish_df %>%
  select(Year, Capelin)

CapelinBiomass[CapelinBiomass == 0] <- NA# replace zeros with NAs
#head(CapelinBiomass)
#


