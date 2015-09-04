################################################################
## Communities of Practice data compilation
## created 24 Aug 2015
## This will replace the spreadsheet started on Google Drive.
################################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)


## create empty data frame with Year column
CoPrct=data.frame('Year'=c(1975:2015))

## Steps for adding data columns: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)
## 3) merge with CoPrct dataframe:   CoPrct=merge(CoPrct,newData,all.x=T)  # rename new df CoPrct

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
# Euphausids:
### NOTE: Need to check these data for NAs, and appropriate aggretation to annual values...
###  this was made as a quick example and hasn't been checked yet.
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
CoPrct <- merge(CoPrct,Euph,all.x=T)


############################################################################################
### Salmon data from ADF&G: http://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisherySalmon.exvesselquery
## orig added by Steph Zador

#library(XML)
#library(httr)
post_url <- "http://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisherySalmon.exvesselquery"
post_body <- list(Year="2014", areas= "allareas", speciesx= "allspecies", submit= "Find%20the%20Data")
html <- POST(post_url, body = post_body, encode = "form")
akcommdf_2014 <- readHTMLTable(content(html))[[1]]

salmonDf<-data.frame(V1= character(0), 
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
  #group_by(year) %>%
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
  #group_by(year) %>%
  mutate(goaKingCatchAveSize=mean(as.numeric(as.character(aveWtLbs)))) %>%
  filter(!duplicated(goaKingCatchAveSize)) %>%
  select(Year,goaKingCatchNum,goaKingCatchLbs,goaKingCatchAveSize)
#

CoPrct <- merge(CoPrct,kingDf,all.x=T)

###############################################################################################
### North Pacific Index (NPI) for sea level pressure: 
URL_npi <- "https://climatedataguide.ucar.edu/sites/default/files/climate_index_files/npindex_monthly.ascii"
npiGet <- GET(URL_npi)
npi1 <- content(npiGet, as='text')
npi <- read.table(file=textConnection(npi1),stringsAsFactors=F, sep=" ", header=TRUE, fill=TRUE)
npi[1:50,]
#
NPI <- npi %>%
           rename(YearMon=X, SeaLevelPressure_hPa=and) %>% # rename columns with data
           select(YearMon, SeaLevelPressure_hPa) %>% # remove columns without data
           mutate(Year=substring(YearMon,1,4),   # creates Year column
                  Month=substring(YearMon,5,6)) %>%  # creates Month column
           filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
           filter(!is.na(SeaLevelPressure_hPa),
                  SeaLevelPressure_hPa != -999.00) %>% # remove NA values, and -999 values which are NAs
           group_by(Year) %>%
           summarise(SeaLevelPressure_mean_hPa=mean(SeaLevelPressure_hPa)) %>% # get annual means
           ungroup() 
#
CoPrct <- merge(CoPrct,NPI,all.x=T) 

###############################################################################################
###  Multivariate ENSO Index (MEI): 
URL_enso <- "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
ensoGet <- htmlParse(URL_enso)
enso1 <- getNodeSet(ensoGet, "//pre")
enso <- read.table(file=textConnection(xmlValue(enso1)),stringsAsFactors=FALSE)

head(enso)

enso_df <- data.frame(enso[11:864,])



# The readHTMLTable() help page provides an example of reading a plain text table out of an 
# HTML <pre> element using htmlParse(), getNodeSet(), textConnection() and read.table() 








