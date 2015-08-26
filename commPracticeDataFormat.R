################################################################
## Communities of Practice data compilation
## created 24 Aug 2015
## This will replace the spreadsheet started on Google Drive.
################################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)

## create empty data frame with Year column
CoPlc=data.frame('Year'=c(1975:2015))

## Steps for adding data columns: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)
## 3) merge with CoPlc dataframe:   CoPlc=merge(CoPlc,newData,all.x=T)  # rename new df CoPlc

# PWS wild Pink Salmon SSB:
URL_Pinks <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uVEZya3VTVnE3Wk0"
PinksGet <- GET(URL_Pinks)
Pinks1 <- content(PinksGet, as='text')
WPinks <- read.csv(file=textConnection(Pinks1),stringsAsFactors=F)
head(WPinks)
#
CoPlc <- merge(CoPlc,WPinks,all.x=T) # merge with CoPlc dataframe

# Euphausids:
URL_Eu <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.61.3"
EuGet <- GET(URL_Eu)
Eu1 <- content(EuGet, as='text')
Eu <- read.csv(file=textConnection(Eu1),stringsAsFactors=F)
head(Eu)
# 
DT <- strsplit(as.character(Eu$startDateTime), split=" ") # split the column to extract year
DT2<- sapply(DT, function(x) x[1])
DY <- strsplit(as.character(DT2), split="-") 
Eu$Year <- sapply(DY, function(x) x[1]) # create Sample Year column
head(Eu)

Euph <- Eu %>% 
        filter(specimen %in% "Euphausiacea") %>%  # select just Euphausids
        group_by(Year) %>%
        summarise(Euph_mn_bmss_g_m3=mean(biomass)) %>%
        ungroup() 
#
CoPlc <- merge(CoPlc,Euph,all.x=T)


### orca data:
