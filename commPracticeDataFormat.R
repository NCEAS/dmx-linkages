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
library(rvest)
library(tidyr)
library(stringr)

#zach=kook...still

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
###  Water Temperature (SST): 
URL_T <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.31.1"
TGet <- GET(URL_T)
T1 <- content(TGet, as='text')
Tmps <- read.csv(file=textConnection(T1),stringsAsFactors=FALSE,strip.white=TRUE)
head(Tmps)

URL_Ts <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.32.2"
TsGet <- GET(URL_Ts)
Ts1 <- content(TsGet, as='text')
TmpSams <- read.csv(file=textConnection(Ts1),stringsAsFactors=FALSE,strip.white=TRUE)
head(TmpSams)
#
Temps <- merge(Tmps, TmpSams, all.x=TRUE)  # merge sample information with data values
Temps$Date <- sapply((strsplit(as.character(Temps$dateTime), split=" ")), function(x) x[1]) # split date out
head(Temps)

############################################
### NOTE : Need to deal with missing sample info for cruiseID TXS09, consecStationNum 3
############################################
# missing_date <- filter(Temps, is.na(dateTime))  # selects data with missing dates
# miss_cID <- unique(missing_date$cruiseID) # selects the cruise IDs for which sample info is missing


SST <- Temps %>%
             mutate(Year=sapply((strsplit(as.character(Date), split="/")), 
                                function(x) x[3])) %>%   # creates Year column
             arrange(dateTime) %>%
             rename(WTemp_C=temp) %>%
             group_by(Year) %>%
             summarise(WTemp_C_AnnMn=mean(WTemp_C)) %>% # get annual means
             ungroup() %>%
             select(Year, WTemp_C_AnnMn)  # selects columns wanted
#
CoPrct <- merge(CoPrct,SST,all.x=T)
  

##############################################################################################
# Winds (annual and winter) : from National Buoy Data Center

# selected buoys in central Gulf of Alaska:
# Station 46076  NDBC  CAPE CLEARE - 17 NM South of Montague Is, AK
# Station 46080  NDBC  PORTLOCK BANK- 76NM ENE of Kodiak, AK
# Station 46078  NDBC  ALBATROSS BANK - 104NM South of Kodiak, AK

### Station 46076 
# NOTE: The 2005 data starts in June
URLS76_05 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2005.txt.gz&dir=data/historical/stdmet/"
S76_05Get <- GET(URLS76_05)
S76_051 <- content(S76_05Get, as='text')
S76_05 <- read.table(file=textConnection(S76_051),stringsAsFactors=FALSE,header=TRUE)

URLS76_06 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2006.txt.gz&dir=data/historical/stdmet/"
S76_06Get <- GET(URLS76_06)
S76_061 <- content(S76_06Get, as='text')
S76_06 <- read.table(file=textConnection(S76_061),stringsAsFactors=FALSE,header=TRUE)

URLS76_07 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2007.txt.gz&dir=data/historical/stdmet/"
S76_07Get <- GET(URLS76_07)
S76_071 <- content(S76_07Get, as='text')
S76_07_h <- scan(textConnection(S76_071), nlines=1, what=character())  # reads first header line
S76_07_h <- gsub("#YY", "YYYY", S76_07_h)  # gets rid of column name with # in it
#S76_07_units <- scan(textConnection(S76_071), skip=1, nlines=1, what=character()) #reads second header line
S76_07 <- read.table(file=textConnection(S76_071),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_07) <- S76_07_h   # pastes the header line in
S76_07 <- rename(S76_07, WD=WDIR, BAR=PRES)

URLS76_08 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2008.txt.gz&dir=data/historical/stdmet/"
S76_08Get <- GET(URLS76_08)
S76_081 <- content(S76_08Get, as='text')
S76_08_h <- scan(textConnection(S76_081), nlines=1, what=character())  # reads first header line
S76_08_h <- gsub("#YY", "YYYY", S76_08_h)  # gets rid of column name with # in it
S76_08 <- read.table(file=textConnection(S76_081),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_08) <- S76_08_h   # pastes the header line in
S76_08 <- rename(S76_08, WD=WDIR, BAR=PRES)
      
URLS76_09 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2009.txt.gz&dir=data/historical/stdmet/"            
S76_09Get <- GET(URLS76_09)
S76_091 <- content(S76_09Get, as='text')
S76_09_h <- scan(textConnection(S76_091), nlines=1, what=character())  # reads first header line
S76_09_h <- gsub("#YY", "YYYY", S76_09_h)  # gets rid of column name with # in it
S76_09 <- read.table(file=textConnection(S76_091),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_09) <- S76_09_h   # pastes the header line in
S76_09 <- rename(S76_09, WD=WDIR, BAR=PRES)

URLS76_10 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2010.txt.gz&dir=data/historical/stdmet/"
S76_10Get <- GET(URLS76_10)
S76_101 <- content(S76_10Get, as='text')
S76_10_h <- scan(textConnection(S76_101), nlines=1, what=character())  # reads first header line
S76_10_h <- gsub("#YY", "YYYY", S76_10_h)  # gets rid of column name with # in it
S76_10 <- read.table(file=textConnection(S76_101),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_10) <- S76_10_h   # pastes the header line in
S76_10 <- rename(S76_10, WD=WDIR, BAR=PRES)

URLS76_11 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2011.txt.gz&dir=data/historical/stdmet/"
S76_11Get <- GET(URLS76_11)
S76_111 <- content(S76_11Get, as='text')
S76_11_h <- scan(textConnection(S76_111), nlines=1, what=character())  # reads first header line
S76_11_h <- gsub("#YY", "YYYY", S76_11_h)  # gets rid of column name with # in it
S76_11 <- read.table(file=textConnection(S76_111),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_11) <- S76_11_h   # pastes the header line in
S76_11 <- rename(S76_11, WD=WDIR, BAR=PRES)

URLS76_12 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2012.txt.gz&dir=data/historical/stdmet/"
S76_12Get <- GET(URLS76_12)
S76_121 <- content(S76_12Get, as='text')
S76_12_h <- scan(textConnection(S76_121), nlines=1, what=character())  # reads first header line
S76_12_h <- gsub("#YY", "YYYY", S76_12_h)  # gets rid of column name with # in it
S76_12 <- read.table(file=textConnection(S76_121),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_12) <- S76_12_h   # pastes the header line in
S76_12 <- rename(S76_12, WD=WDIR, BAR=PRES)

URLS76_13 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2013.txt.gz&dir=data/historical/stdmet/"
S76_13Get <- GET(URLS76_13)
S76_131 <- content(S76_13Get, as='text')
S76_13_h <- scan(textConnection(S76_131), nlines=1, what=character())  # reads first header line
S76_13_h <- gsub("#YY", "YYYY", S76_13_h)  # gets rid of column name with # in it
S76_13 <- read.table(file=textConnection(S76_131),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_13) <- S76_13_h   # pastes the header line in
S76_13 <- rename(S76_13, WD=WDIR, BAR=PRES)

URLS76_14 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2014.txt.gz&dir=data/historical/stdmet/"
S76_14Get <- GET(URLS76_14)
S76_141 <- content(S76_14Get, as='text')
S76_14_h <- scan(textConnection(S76_141), nlines=1, what=character())  # reads first header line
S76_14_h <- gsub("#YY", "YYYY", S76_14_h)  # gets rid of column name with # in it
S76_14 <- read.table(file=textConnection(S76_141),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S76_14) <- S76_14_h   # pastes the header line in
S76_14 <- rename(S76_14, WD=WDIR, BAR=PRES)

# bind all years into one data frame
CC <- bind_rows(S76_05,S76_06,S76_07,S76_08,S76_09,S76_10,S76_11,S76_12,S76_13,S76_14)
head(CC) ; str(CC)

### Station 46080
# NOTE: Data starts in August of 2002
URLS80_02 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2002.txt.gz&dir=data/historical/stdmet/"
S80_02Get <- GET(URLS80_02)
S80_021 <- content(S80_02Get, as='text')
S80_02 <- read.table(file=textConnection(S80_021),stringsAsFactors=FALSE,header=TRUE)

URLS80_03 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2003.txt.gz&dir=data/historical/stdmet/"
S80_03Get <- GET(URLS80_03)
S80_031 <- content(S80_03Get, as='text')
S80_03 <- read.table(file=textConnection(S80_031),stringsAsFactors=FALSE,header=TRUE)

URLS80_04 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2004.txt.gz&dir=data/historical/stdmet/"
S80_04Get <- GET(URLS80_04)
S80_041 <- content(S80_04Get, as='text')
S80_04 <- read.table(file=textConnection(S80_041),stringsAsFactors=FALSE,header=TRUE)

URLS80_05 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2005.txt.gz&dir=data/historical/stdmet/"
S80_05Get <- GET(URLS80_05)
S80_051 <- content(S80_05Get, as='text')
S80_05 <- read.table(file=textConnection(S80_051),stringsAsFactors=FALSE,header=TRUE)

URLS80_06 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2006.txt.gz&dir=data/historical/stdmet/"
S80_06Get <- GET(URLS80_06)
S80_061 <- content(S80_06Get, as='text')
S80_06 <- read.table(file=textConnection(S80_061),stringsAsFactors=FALSE,header=TRUE)

URLS80_07 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2007.txt.gz&dir=data/historical/stdmet/"
S80_07Get <- GET(URLS80_07)
S80_071 <- content(S80_07Get, as='text')
S80_07_h <- scan(textConnection(S80_071), nlines=1, what=character())  # reads first header line
S80_07_h <- gsub("#YY", "YYYY", S80_07_h)  # gets rid of column name with # in it
S80_07 <- read.table(file=textConnection(S80_071),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_07) <- S80_07_h   # pastes the header line in
S80_07 <- rename(S80_07, WD=WDIR, BAR=PRES)

URLS80_08 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2008.txt.gz&dir=data/historical/stdmet/"
S80_08Get <- GET(URLS80_08)
S80_081 <- content(S80_08Get, as='text')
S80_08_h <- scan(textConnection(S80_081), nlines=1, what=character())  # reads first header line
S80_08_h <- gsub("#YY", "YYYY", S80_08_h)  # gets rid of column name with # in it
S80_08 <- read.table(file=textConnection(S80_081),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_08) <- S80_08_h   # pastes the header line in
S80_08 <- rename(S80_08, WD=WDIR, BAR=PRES)

URLS80_09 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2009.txt.gz&dir=data/historical/stdmet/"
S80_09Get <- GET(URLS80_09)
S80_091 <- content(S80_09Get, as='text')
S80_09_h <- scan(textConnection(S80_091), nlines=1, what=character())  # reads first header line
S80_09_h <- gsub("#YY", "YYYY", S80_09_h)  # gets rid of column name with # in it
S80_09 <- read.table(file=textConnection(S80_091),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_09) <- S80_09_h   # pastes the header line in
S80_09 <- rename(S80_09, WD=WDIR, BAR=PRES)

URLS80_10 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2010.txt.gz&dir=data/historical/stdmet/"
S80_10Get <- GET(URLS80_10)
S80_101 <- content(S80_10Get, as='text')
S80_10_h <- scan(textConnection(S80_101), nlines=1, what=character())  # reads first header line
S80_10_h <- gsub("#YY", "YYYY", S80_10_h)  # gets rid of column name with # in it
S80_10 <- read.table(file=textConnection(S80_101),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_10) <- S80_10_h   # pastes the header line in
S80_10 <- rename(S80_10, WD=WDIR, BAR=PRES)

URLS80_11 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2011.txt.gz&dir=data/historical/stdmet/"
S80_11Get <- GET(URLS80_11)
S80_111 <- content(S80_11Get, as='text')
S80_11_h <- scan(textConnection(S80_111), nlines=1, what=character())  # reads first header line
S80_11_h <- gsub("#YY", "YYYY", S80_11_h)  # gets rid of column name with # in it
S80_11 <- read.table(file=textConnection(S80_111),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_11) <- S80_11_h   # pastes the header line in
S80_11 <- rename(S80_11, WD=WDIR, BAR=PRES)

URLS80_12 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2012.txt.gz&dir=data/historical/stdmet/"
S80_12Get <- GET(URLS80_12)
S80_121 <- content(S80_12Get, as='text')
S80_12_h <- scan(textConnection(S80_121), nlines=1, what=character())  # reads first header line
S80_12_h <- gsub("#YY", "YYYY", S80_12_h)  # gets rid of column name with # in it
S80_12 <- read.table(file=textConnection(S80_121),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_12) <- S80_12_h   # pastes the header line in
S80_12 <- rename(S80_12, WD=WDIR, BAR=PRES)

# NOTE: 2013 data missing from online http://www.ndbc.noaa.gov/station_history.php?station=46080

URLS80_14 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2014.txt.gz&dir=data/historical/stdmet/"
S80_14Get <- GET(URLS80_14)
S80_141 <- content(S80_14Get, as='text')
S80_14_h <- scan(textConnection(S80_141), nlines=1, what=character())  # reads first header line
S80_14_h <- gsub("#YY", "YYYY", S80_14_h)  # gets rid of column name with # in it
S80_14 <- read.table(file=textConnection(S80_141),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S80_14) <- S80_14_h   # pastes the header line in
S80_14 <- rename(S80_14, WD=WDIR, BAR=PRES)

# bind all years into one data frame
PB <- bind_rows(S80_02,S80_03,S80_04,S80_05,S80_06,S80_07,S80_08,S80_09,S80_10,S80_11,S80_12,S80_14)
head(PB) ; str(PB)

### Station 46078
# NOTE: Data starts in May 2004
URLS78_04 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2004.txt.gz&dir=data/historical/stdmet/"
S78_04Get <- GET(URLS78_04)
S78_041 <- content(S78_04Get, as='text')
S78_04 <- read.table(file=textConnection(S78_041),stringsAsFactors=FALSE,header=TRUE)

URLS78_05 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2005.txt.gz&dir=data/historical/stdmet/"
S78_05Get <- GET(URLS78_05)
S78_051 <- content(S78_05Get, as='text')
S78_05 <- read.table(file=textConnection(S78_051),stringsAsFactors=FALSE,header=TRUE)

URLS78_06 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2006.txt.gz&dir=data/historical/stdmet/"
S78_06Get <- GET(URLS78_06)
S78_061 <- content(S78_06Get, as='text')
S78_06 <- read.table(file=textConnection(S78_061),stringsAsFactors=FALSE,header=TRUE)

URLS78_07 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2007.txt.gz&dir=data/historical/stdmet/"
S78_07Get <- GET(URLS78_07)
S78_071 <- content(S78_07Get, as='text')
S78_07_h <- scan(textConnection(S78_071), nlines=1, what=character())  # reads first header line
S78_07_h <- gsub("#YY", "YYYY", S78_07_h)  # gets rid of column name with # in it
S78_07 <- read.table(file=textConnection(S78_071),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_07) <- S78_07_h   # pastes the header line in
S78_07 <- rename(S78_07, WD=WDIR, BAR=PRES)

URLS78_08 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2008.txt.gz&dir=data/historical/stdmet/"
S78_08Get <- GET(URLS78_08)
S78_081 <- content(S78_08Get, as='text')
S78_08_h <- scan(textConnection(S78_081), nlines=1, what=character())  # reads first header line
S78_08_h <- gsub("#YY", "YYYY", S78_08_h)  # gets rid of column name with # in it
S78_08 <- read.table(file=textConnection(S78_081),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_08) <- S78_08_h   # pastes the header line in
S78_08 <- rename(S78_08, WD=WDIR, BAR=PRES)

URLS78_09 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2009.txt.gz&dir=data/historical/stdmet/"
S78_09Get <- GET(URLS78_09)
S78_091 <- content(S78_09Get, as='text')
S78_09_h <- scan(textConnection(S78_091), nlines=1, what=character())  # reads first header line
S78_09_h <- gsub("#YY", "YYYY", S78_09_h)  # gets rid of column name with # in it
S78_09 <- read.table(file=textConnection(S78_091),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_09) <- S78_09_h   # pastes the header line in
S78_09 <- rename(S78_09, WD=WDIR, BAR=PRES)

URLS78_10 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2010.txt.gz&dir=data/historical/stdmet/"
S78_10Get <- GET(URLS78_10)
S78_101 <- content(S78_10Get, as='text')
S78_10_h <- scan(textConnection(S78_101), nlines=1, what=character())  # reads first header line
S78_10_h <- gsub("#YY", "YYYY", S78_10_h)  # gets rid of column name with # in it
S78_10 <- read.table(file=textConnection(S78_101),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_10) <- S78_10_h   # pastes the header line in
S78_10 <- rename(S78_10, WD=WDIR, BAR=PRES)

URLS78_11 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2011.txt.gz&dir=data/historical/stdmet/"
S78_11Get <- GET(URLS78_11)
S78_111 <- content(S78_11Get, as='text')
S78_11_h <- scan(textConnection(S78_111), nlines=1, what=character())  # reads first header line
S78_11_h <- gsub("#YY", "YYYY", S78_11_h)  # gets rid of column name with # in it
S78_11 <- read.table(file=textConnection(S78_111),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_11) <- S78_11_h   # pastes the header line in
S78_11 <- rename(S78_11, WD=WDIR, BAR=PRES)

URLS78_12 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2012.txt.gz&dir=data/historical/stdmet/"
S78_12Get <- GET(URLS78_12)
S78_121 <- content(S78_12Get, as='text')
S78_12_h <- scan(textConnection(S78_121), nlines=1, what=character())  # reads first header line
S78_12_h <- gsub("#YY", "YYYY", S78_12_h)  # gets rid of column name with # in it
S78_12 <- read.table(file=textConnection(S78_121),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_12) <- S78_12_h   # pastes the header line in
S78_12 <- rename(S78_12, WD=WDIR, BAR=PRES)

URLS78_13 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2013.txt.gz&dir=data/historical/stdmet/"
S78_13Get <- GET(URLS78_13)
S78_131 <- content(S78_13Get, as='text')
S78_13_h <- scan(textConnection(S78_131), nlines=1, what=character())  # reads first header line
S78_13_h <- gsub("#YY", "YYYY", S78_13_h)  # gets rid of column name with # in it
S78_13 <- read.table(file=textConnection(S78_131),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_13) <- S78_13_h   # pastes the header line in
S78_13 <- rename(S78_13, WD=WDIR, BAR=PRES)

URLS78_14 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2014.txt.gz&dir=data/historical/stdmet/"
S78_14Get <- GET(URLS78_14)
S78_141 <- content(S78_14Get, as='text')
S78_14_h <- scan(textConnection(S78_141), nlines=1, what=character())  # reads first header line
S78_14_h <- gsub("#YY", "YYYY", S78_14_h)  # gets rid of column name with # in it
S78_14 <- read.table(file=textConnection(S78_141),stringsAsFactors=FALSE,skip=2,header=FALSE)
names(S78_14) <- S78_14_h   # pastes the header line in
S78_14 <- rename(S78_14, WD=WDIR, BAR=PRES)

# bind all years into one data frame
AB <- bind_rows(S78_04,S78_05,S78_06,S78_07,S78_08,S78_09,S78_10,S78_11,S78_12,S78_13,S78_14)
head(AB) ; str(AB)

# bind data from three buoys together
Buoys_all <- bind_rows(CC,PB,AB)

# 
Wind_Ann <- Buoys_all %>% 
                  filter(WD!=99, WD!=999, WSPD!=99, WSPD!=999) %>%  # remove missing data
                  rename(Year=YYYY) %>%       # rename column for uniformity
                  group_by(Year) %>%
                  summarise(WndDir_degT_AnnMn=mean(WD),WndSp_m_s_AnnMn=mean(WSPD)) %>%  # get means
                  ungroup() %>%
                  select(Year, WndDir_degT_AnnMn, WndSp_m_s_AnnMn) # select wind direction and speed

Wind_Winter <- Buoys_all %>%
                     filter(WD!=99, WD!=999, WSPD!=99, WSPD!=999) %>%  # remove missing data
                     rename(Year=YYYY) %>%       # rename column for uniformity
                     filter(MM %in% c(12,1,2)) %>%
                     group_by(Year) %>%
                     summarise(WndDir_degT_Winter=mean(WD),WndSp_m_s_Winter=mean(WSPD)) %>%  # get means
                     ungroup() %>%
                     select(Year, WndDir_degT_Winter, WndSp_m_s_Winter)  # select wind direction and speed
#
CoPrct <- merge(CoPrct,Wind_Ann,all.x=T)  # Annual mean wind
CoPrct <- merge(CoPrct,Wind_Winter,all.x=T)  # Winter mean wind

                  
###############################################################################################
###  Multivariate ENSO Index (MEI): 
URL_enso <- "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
enso_pre <- xpathSApply(content(GET(URL_enso)),"/html/body/pre", xmlValue)
enso_cols <- scan(textConnection(enso_pre), skip=10, nlines=1, what=character()) # get header row
enso <- read.csv(file=textConnection(enso_pre), skip=11, stringsAsFactors=F, sep="\t", 
                 header=FALSE, col.names=enso_cols)
enso_df <- enso[1:66,]  # removes the text at bottom of file
#
ENSO_annual <- enso_df %>%
                    rename(Year=YEAR) %>% # rename data columns
                    filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
                    gather(Months, ENSO, -Year) %>% # reshapes data to be column-wise
                    filter(!is.na(ENSO)) %>% # remove NA values
                    group_by(Year) %>%
                    summarise(ENSO_anul_mn=mean(ENSO)) %>% # get annual means
                    ungroup()  # 
# 
CoPrct <- merge(CoPrct,ENSO_annual,all.x=T) 

# This was my initial try, which didn't work: 
# The readHTMLTable() help page provides an example of reading a plain text table out of an 
# HTML <pre> element using htmlParse(), getNodeSet(), textConnection() and read.table() 

##############################################################################################
# North Pacific Gyre Oscillation Index (NPGO): 
URL_npgo <- "http://www.o3d.org/npgo/npgo.php"
npgo_pre <- xpathSApply(content(GET(URL_npgo)),"/html/body/pre", xmlValue)
npgo_cols <- scan(textConnection(npgo_pre), skip=25, nlines=1, what=character())# Get header row
npgo_cols <- npgo_cols[2:4] # select column names
npgo_df <- read.csv(file=textConnection(npgo_pre), skip=26, stringsAsFactors=F, sep="", 
                 header=FALSE, col.names=npgo_cols, strip.white=TRUE)

npgo_annual <- npgo_df %>% 
               rename(Year=YEAR) %>% # rename data columns         
               filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
               group_by(Year) %>%
               summarise(NPGO_anul_mn=mean(NPGO)) %>% # get annual means
               ungroup()  # 
#
CoPrct <- merge(CoPrct,npgo_annual,all.x=T) 

##############################################################################################
###  Pacific Decadal Oscillation Index (PDO): 
URL_pdo <- "http://jisao.washington.edu/pdo/PDO.latest"
pdo_raw <- html(URL_pdo)
pdo_pre <- pdo_raw %>% 
               html_node("p") %>%
               html_text()
pdo_cols <- scan(textConnection(pdo_pre), skip=29, nlines=1, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_pre), skip=30, nrows=116, stringsAsFactors=F, sep="", 
                  header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4)  # removes asterisks from years 2002-2015

pdo_annual <- pdo_df %>% 
              rename(Year=YEAR) %>% # rename data columns         
              filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
              gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
              group_by(Year) %>%
              summarise(PDO_anul_mn=mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get annual means
              ungroup() 
#
CoPrct <- merge(CoPrct,pdo_annual,all.x=T)

###############################################################################################
### Upwelling Anomalies: 
URL_upanom <- "http://www.pfeg.noaa.gov/products/PFELData/upwell/monthly/upanoms.mon"
upanom_raw <- html(URL_upanom)
upanom_pre <- upanom_raw %>% 
              html_node("p") %>%
              html_text()
upanom_cols <- scan(textConnection(upanom_pre), skip=2, nlines=1, what=character())# Get header row
upanom_cols <- c("Lat", "Long", upanom_cols[-1])# split position into lat and long 
upanom_df <- read.csv(file=textConnection(upanom_pre), skip=4, stringsAsFactors=F, sep="", 
                   header=FALSE, col.names=upanom_cols, strip.white=TRUE)
#
upanom <- upanom_df %>% 
          rename(Year=YEAR) %>% # rename data columns   
          filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
          gather(Month, UpwelAnom,-Year,-Lat,-Long) %>% # reshapes data to be column-wise
          group_by(Year) %>%
          summarise(UpWelAnom_anul_mn=mean(UpwelAnom, na.rm = TRUE)) %>% # get annual means
          ungroup() 
#
CoPrct <- merge(CoPrct,upanom,all.x=T)

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

###############################################################################################
### Pollock Biomass (from NOAA stock assessments):

# https://github.com/gimoya/theBioBucket-Archives/blob/master/R/txtmining_pdf.R
# download pdftotxt from 
# ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip
# and extract to your program files folder

#library(tm)

#URL_Pollock <- "http://www.afsc.noaa.gov/REFM/Docs/2014/GOApollock.pdf"
#PollGet <- GET(URL_Pollock)
#readPDF(engine="xpdf",)

URL_T19 <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-udFBubXBZTDhXUDA"
PollGet <- GET(URL_T19)
Poll1 <- content(PollGet, as='text')
Poll_df <- read.csv(file=textConnection(Poll1),stringsAsFactors=FALSE)
head(Poll_df)
#
Poll_Adult <- Poll_df %>%
              rename(Poll_Yr3plus_TtlBmss_1000Tons=X3..total.biomass...1.000.t., 
                     Catch_tons=Catch..t.) %>%
              filter(!is.na(Year)) %>%
              select(Year, Poll_Yr3plus_TtlBmss_1000Tons)

Poll_Yr1 <- Poll_df %>%
            rename(Poll_Age1_recruits_millions=Age.1.recruits..million.,
                   Catch_tons=Catch..t.) %>%
            filter(!is.na(Year)) %>%
            select(Year, Poll_Age1_recruits_millions)
            
#
CoPrct <- merge(CoPrct, Poll_Adult,all.x=T) # adult pollock
CoPrct <- merge(CoPrct, Poll_Yr1,all.x=T) # year 1 pollock

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
########################################################################################################
### Pacific Cod (from NOAA stock assessments): 
# Metadata column header: female spawning biomass from Table 2.18 – Estimated female spawning biomass (t) 
#                                               from the 2012 assessment and this year’s assessment
#                         Age 1 numbers in millins from Table Table 2.20 – Estimated numbers-at-age 
#                                              (millions) at the time of spawning

URL_PC <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-ubDJJdkE4NVg4a28"
PC_Get <- GET(URL_PC)
PC1 <- content(PC_Get, as='text')
PC_df <- read.csv(file=textConnection(PC1),stringsAsFactors=FALSE,head=TRUE)
head(PC_df)
#
PC_df <- PC_df %>%
         rename(Year=year, PCod_female_Bmss_t=female.spawning.biomass, 
                PCod_Age1_millions=Age.1.numbers..in.millions.)
     
#
CoPrct <- merge(CoPrct,PC_df,all.x=T)  

########################################################################################################
### Eddy Kinetic Energy (EKE): 
# http://www.gfdl.noaa.gov/wcrp2011_poster_c37_dixon_th85b_eke
# Metadata column header: Annual Maximum EKE in each region
#                         Region B: Lat-56.5N to 57.5N, Lon-139W to 137W
#                         Region C: Lat-58N to 59N, Lon- 148W to 146W
#                         Region D: Lat-54.8N to 55.8N, Lon-155.5W to 153.5W

URL_EKE <- "https://drive.google.com/uc?export=download&id=0By1iaulIAI-uaUExWmkwUk93V0k"
EKE_Get <- GET(URL_EKE)
EKE1 <- content(EKE_Get, as='text')
EKE_df <- read.csv(file=textConnection(EKE1),stringsAsFactors=FALSE,head=TRUE)
head(EKE_df)
#
EKE <- EKE_df %>%
       rename(Region_D=Region.d, Region_C=Region.c, Region_B=Region.b) %>%
       gather(Region, EKE_ann_max, -Year) %>% # reshapes data to be column-wise
       group_by(Year) %>%
       summarize(EKE_ann_max_mean=mean(EKE_ann_max, na.rm=TRUE)) %>% # get annual means
       ungroup()
#
CoPrct <- merge(CoPrct,EKE,all.x=T)  

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
CoPrct <- merge(CoPrct,ShSk,all.x=T) 
#########################################################################################################
# Tanner Crab
# Data are from Spalinger 2015 (ADFG Fishery Management Report No. 15-27):
# cite as:
# Spalinger, K. 2015. Bottom trawl survey of crab and groundfish: Kodiak, Chignik, 
# South Peninsula, and Eastern Aleutian Management Districts, 2014. Alaska Department of 
# Fish and Game, Fishery Management Report No. 15-27, Anchorage.
# Data are abundances pulled from Tables 3 & 7
URL_TCrab <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7udDlyLTU4dDlwa0U"
TCrab_Get <- GET(URL_TCrab)
TCrab1 <- content(TCrab_Get, as='text')
TCrab_df <- read.csv(file=textConnection(TCrab1),stringsAsFactors=FALSE,head=TRUE)
head(TCrab_df)
#
TCrab = TCrab_df %>%
  filter(Year != 1988) %>% # some sites not sampled in 1988
  filter(Year != 1995) %>% # some sites not sample in 1995
  mutate(TotTCrab = KodiakTotalTannerCrabAbund + WesternSectionTotals + 
           EasternSectionTotals + SouthPeninsulaDistrictTotals + ChignikDistrictTotals)
View(TCrab)
CoPrct <- merge(CoPrct,TCrab,all.x=T)
#########################################################################################################
#

