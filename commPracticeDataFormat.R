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



# Optional: Write data frame to a CSV
#write.csv(CoPrct, file = "CoPrct.csv", row.names=FALSE)


############################################################################################
# PWS wild Pink Salmon SSB:
URL_Pinks <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uVEZya3VTVnE3Wk0"
PinksGet <- GET(URL_Pinks)
Pinks1 <- content(PinksGet, as='text')
WPinks <- read.csv(file=textConnection(Pinks1),stringsAsFactors=F)
head(WPinks)
#
CoPrct <- merge(CoPrct,WPinks,all.x=T) # merge with CoPrct dataframe

############################################################################################
### Salmon data from ADF&G: http://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisherySalmon.exvesselquery
## orig added by Steph Zador

post_url <- "http://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisherySalmon.exvesselquery"
post_body <- list(Year="2014", areas= "allareas", speciesx= "allspecies", submit= "Find%20the%20Data")
html <- POST(post_url, body = post_body, encode = "form")
akcommdf_2014 <- readHTMLTable(content(html))[[1]]

salmonDf<-data.frame(V1 = character(0),
                     V2 = character(0),
                     V3 = character(0),
                     V4 = character(0),
                     V5 = character(0),
                     V6 = character(0),
                     Year=character(0))
Year=rep(1994:2014,6)
selectionDf=as.data.frame(Year) %>%
  arrange(Year) %>%
  mutate(area=rep(c('southeast','Prince William Sound','Cook Inlet','kodiak','chignik','AK Peninsula / Aleutian Is.'),21))

acc=1
for(i in 1:nrow(selectionDf)) {
  post_body <- list(Year=selectionDf[i,1], areas=selectionDf[i,2], speciesx='allspecies', submit= "Find%20the%20Data")
  html <- POST(post_url, body = post_body, encode = "form")
  akSalmonDf <- as.data.frame(readHTMLTable(content(html))[[1]])
  akSalmonDf$Year<-selectionDf[i,1]
  salmonDf=rbind(salmonDf,akSalmonDf)
  acc=acc+1
}
colnames(salmonDf)=c('species','aveWtLbs','avePricePerLb','nFishInThousands','lbsFishInThousands','estValueInThousands','Year')

######### Pink salmon catch data #########

pinkDf=salmonDf %>%
  filter(species=='Pink') %>%
  group_by(Year)%>%
  mutate(goaPinkCatchNum=sum(as.numeric(gsub(',','',nFishInThousands)))) %>%
  mutate(goaPinkCatchLbs=sum(as.numeric(gsub(',','',lbsFishInThousands)))) %>%
  #group_by(Year) %>%
  mutate(goaPinkCatchAveSize=mean(as.numeric(as.character(aveWtLbs)))) %>%
  filter(!duplicated(goaPinkCatchAveSize)) %>%
  select(Year,goaPinkCatchNum,goaPinkCatchLbs,goaPinkCatchAveSize)
#

CoPrct <- merge(CoPrct,pinkDf,all.x=T)

######### King/Chinkook salmon data #########

kingDf=salmonDf %>%
  filter(species %in% c('Chinook','Chinookd','Chinookc')) %>%
  group_by(Year)%>%
  mutate(goaKingCatchNum=sum(na.omit(as.numeric(gsub(',','',nFishInThousands))))) %>%
  mutate(goaKingCatchLbs=sum(as.numeric(gsub(',','',lbsFishInThousands)))) %>%
  #group_by(Year) %>%
  mutate(goaKingCatchAveSize=mean(as.numeric(as.character(aveWtLbs)))) %>%
  filter(!duplicated(goaKingCatchAveSize)) %>%
  select(Year,goaKingCatchNum,goaKingCatchLbs,goaKingCatchAveSize)
#

CoPrct <- merge(CoPrct,kingDf,all.x=T)


###############################################################################################
### Stellar Sea Lions:
URL_SSL <- "https://goa.nceas.ucsb.edu/goa/metacat?action=read&qformat=metacatui&sessionid=&docid=df35b.270.1"
SSLGet <- GET(URL_SSL)
SSL1 <- content(SSLGet, as='text')
SSL_df <- read.csv(file=textConnection(SSL1),stringsAsFactors=FALSE)
head(SSL_df)
#
# NOTE: This is just means of the count data in the Central GOA region.
SSL <- SSL_df %>%
     #  filter(counttype %in% c(2,3)) %>% # selects count types 2 & 3, which are vertical photos,
       rename(Year=year) %>%               # and according to the metadata were the most accurate
       filter(region=="C GULF", Year %in% c(1975:2015)) %>% # selects years 1975 - 2015), selects central GOA counts
       arrange(Year) %>%
       group_by(Year) %>%
       summarise(SSLnonPup_anul_mn=mean(adult.juvenileCount, na.rm=TRUE)) %>% # get annual means
       ungroup()

#####
# This is code for trying to use the count data to calculate interpolated regional abundances.
#  I can't get it to work!!!

#install.packages("devtools")
#devtools::install_github("NMML/agTrend")
#library(agTrend)
#
#SSL_fit <- mcmc.aggregate(start=1975, end=2015, data=SSL, aggregation="region",
#                          abund.name="adult.juvenileCount", time.name="Year",
#                          site.name="sitename", burn=1000, iter=5000,
#                          keep.site.abund=TRUE, model.data=NULL)
#####

CoPrct <- merge(CoPrct,SSL,all.x=T)

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
########################################################################################################
# Shrimp biomass (units = ?)
# from NOAA / ADFG Small mesh trawl survey
# estimates sent to Stephai Zador from Dan Urban
#
URL_Shr <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7ucFliX1NOZHRMOWc"
ShrGet <- GET(URL_Shr)
Shr1 <- content(ShrGet, as='text')
Shr_df <- read.csv(file=textConnection(Shr1),stringsAsFactors=FALSE)
head(Shr_df)
#
Shr_df <- Shr_df %>%
          rename(Year=year, Pink_Shrimp=p.shrimp)
#
CoPrct <- merge(CoPrct,Shr_df,all.x=T)
#
########################################################################################################
### Arrowtooth adult biomass (age-3 plus; tons)
# from 2013 SAFE Stock Assessment
#
URL_Arr <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uSVh6Rl9FNXFwRm8"
ArrGet <- GET(URL_Arr)
Arr1 <- content(ArrGet, as='text')
Arr_df <- read.csv(file=textConnection(Arr1),stringsAsFactors=FALSE)
head(Arr_df)
#
CoPrct <- merge(CoPrct,Arr_df,all.x=T)
#
#########################################################################################################
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
head(ShSk_df)
#
CoPrct <- merge(CoPrct,ShSk_df,all.x=T)

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

