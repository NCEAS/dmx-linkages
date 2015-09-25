################################################################
## Communities of Practice data compilation script
## created 24 Aug 2015
## This will replace the spreadsheet started on Google Drive.
################################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)

## Steps for adding data columns:
## 1) create empty data frame
## 2) run each data cleaning script to generate data frames
## 3) merge all data frames with CoPrct dataframe:   
##        CoPrct=merge(CoPrct,newData,all.x=T)  


# Create empty data frame with Year column
CoPrct=data.frame('Year'=c(1975:2015))

# Call and run each data cleaning script
          # source the scripts
          # run each script to generate a data frame
          # put all those data frames somewhere so that they can be merged later

# Merge all data frames into one large data frame
CoPrct <- merge(CoPrct,NPI,all.x=T) # North Pacific Index of sea level pressure
CoPrct <- merge(CoPrct,Wind_Ann,all.x=T)  # Annual mean wind - Central GOA
CoPrct <- merge(CoPrct,Wind_Winter,all.x=T)  # Winter mean wind - Central GOA
CoPrct <- merge(CoPrct,ENSO_annual,all.x=T)   # ENSO annual index
CoPrct <- merge(CoPrct,npgo_annual,all.x=T)  # North Pacific Gyre Oscillation
CoPrct <- merge(CoPrct,pdo_annual,all.x=T)   # Pacific Decadal Oscillation
CoPrct <- merge(CoPrct,upanom,all.x=T)    # Pacific Upwelling Anomalies
CoPrct <- merge(CoPrct,PC_df,all.x=T)    # Pacific Cod Stock Assessment
CoPrct <- merge(CoPrct,EKE,all.x=T)    # Eddy Kinetic Energy - Central GOA
CoPrct <- merge(CoPrct,TCrab,all.x=T)    # Tanner Crab Abundance 
CoPrct <- merge(CoPrct,SST,all.x=T)   # Water Temperatures from Seward Line CTD
CoPrct <- merge(CoPrct, Poll_Adult,all.x=T) # Adult Pollock
CoPrct <- merge(CoPrct, Poll_Yr1,all.x=T) # Year 1 Pollock
CoPrct <- merge(CoPrct,Arr_df,all.x=T)   # Arrowtooth adult biomass 
CoPrct <- merge(CoPrct,WPinks,all.x=T) # PWS pink salmon SSB
CoPrct <- merge(CoPrct,pinkDf,all.x=T)  # Pink salmon catch data
CoPrct <- merge(CoPrct,kingDf,all.x=T)  # King salmon catch data
CoPrct <- merge(CoPrct,Shr_df,all.x=T)   # Pink shrimp 
CoPrct <- merge(CoPrct,ShSk_df,all.x=T)  # Sharks and Skates
CoPrct <- merge(CoPrct,SSL,all.x=T)    # Stellar Sea Lions







# Optional: Write data frame to a CSV
#write.csv(CoPrct, file = "CoPrct.csv", row.names=FALSE)


###############################################################################################
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
View(MayZo)
#
MayCopepods <- MayZo %>%
               mutate(MayCopepods = rowSums(MayZo[,2:35], na.rm=T)) %>% # sum all small & large copepods
               select(Year, MayCopepods)
head(MayCopepods)
#
CoPrct <- merge(CoPrct,MayCopepods,all.x=T)
#
MayEuphausiids <- MayZo %>%
                  select(Year, Euphausiids)
head(MayEuphausiids)
#
CoPrct <- merge(CoPrct,MayEuphausiids,all.x=T)

###############################################################################################
### Fish data from Sarah Gaichas
# NB Sarah cautions that these data should be used only as placeholders until they're updated
#
URL_Fish <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ubGJLYXQwRlR0Ujg"
FishGet <- GET(URL_Fish)
Fish1 <- content(FishGet, as='text')
Fish_df <- read.csv(file=textConnection(Fish1),stringsAsFactors=FALSE)
head(Fish_df)
names(Fish_df)
#
# extract Capelin biomass
# units are Biomass (tons/km2) from surveys
CapelinBiomass <- Fish_df %>%
  select(Year, Capelin)
head(CapelinBiomass)
#
CoPrct <- merge(CoPrct,CapelinBiomass,all.x=T)

########################################################################################################
# Mean annual Chl a anomalies (mg/m3) for Gulf of Alaska
# From Waite & Mueter 2013, Fig 11 Annual
# Waite, J.N. and Mueter, F.J. 2013. Spatial and temporal variability of chlorophyll-a concentrations
# in the coastal Gulf of Alaska, 1998-2011, using cloud-free reconstructions of SeaWiFS and MODIS-Aqua data.
# Prog. Oceanogr. 116, 179-192.
#
URL_Chl <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uRHdOTGQtSVBQOE0"
ChlGet <- GET(URL_Chl)
Chl1 <- content(ChlGet, as='text')
Chl_df <- read.csv(file=textConnection(Chl1),stringsAsFactors=FALSE)
head(Chl_df)
#
CoPrct <- merge(CoPrct,Chl_df,all.x=T)
#

#########################################################################################################
# Halibut Fishery Data
URL_HlbtFishery <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uY0JVVHQxSWxXYVU"
HlbtFishery_Get <- GET(URL_HlbtFishery)
HlbtFishery1 <- content(HlbtFishery_Get, as='text')
HlbtFishery_df <- read.csv(file=textConnection(HlbtFishery1),stringsAsFactors=FALSE,head=TRUE)
head(HlbtFishery_df)
#
CoPrct <- merge(CoPrct,HlbtFishery_df,all.x=T)
#########################################################################################################
# Pollock Fishery Data
URL_PollFishery <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ual9URUl0THRBUGs"
PollFishery_Get <- GET(URL_PollFishery)
PollFishery1 <- content(PollFishery_Get, as='text')
PollFishery_df <- read.csv(file=textConnection(PollFishery1),stringsAsFactors=FALSE,head=TRUE)
head(PollFishery_df)
#
CoPrct <- merge(CoPrct,PollFishery_df,all.x=T)
#########################################################################################################

