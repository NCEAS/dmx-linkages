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

# get the html pre element text
enso1 <- html(URL_enso)
enso_content <- enso1 %>% 
                html_node("pre") %>%
                html_text()
# loop through the lines until finding the line starting with "YEAR" and get the column headers
enso_lines <- unlist(strsplit(enso_content, split="\n"))  ## split the string at the new lines
i=1
header_line <- strsplit(enso_lines[i], split="\t")
while (grepl("YEAR", header_line) == F) {header_line <- strsplit(enso_lines[i], split=" +")
                                         i=i+1
                                         }
# loop through the table to gather the data
new_line <- strsplit(enso_lines[i], split="\t")
data_lines <- unlist(strsplit(enso_lines[i], split="\t"))
while (grepl("2015", new_line) == F) {new_line <- data.frame(strsplit(enso_lines[i+1], split="\t"))
                                      #new_line <- data.frame(new_line)
                                      data_lines <- rbind.fill(data_lines, unlist(new_line))
                                      i=i+1
                                      }

enso <- rbind.data.frame(data_lines) # create the dataframe
names(enso) <- matrix(header_line[[1]]) # assign the column name
enso[,c(2:13)] <- as.numeric(as.character(unlist(enso[,c(2:13)])))# make data columns numeric
head(enso)
#
ENSO_annual <- enso %>%
                    rename(Year=YEAR) %>% # rename data columns
                    filter(Year %in% c(1975:2015)) %>% # selects years 1975 - 2015
                    gather(Months, ENSO_anul_mn, -Year) %>% # reshapes data to be column-wise
                #    filter(!is.na(SeaLevelPressure_hPa),
                #                  SeaLevelPressure_hPa != -999.00) %>% # remove NA values, and -999 values which are NAs
                    group_by(Year) %>%
                #    summarise(SeaLevelPressure_mean_hPa=mean(SeaLevelPressure_hPa)) %>% # get annual means
                    ungroup()  # 








# The readHTMLTable() help page provides an example of reading a plain text table out of an 
# HTML <pre> element using htmlParse(), getNodeSet(), textConnection() and read.table() 

##############################################################################################
###  Pacific Decadal Oscillation Index (PDO): 
URL_pdo <- "http://jisao.washington.edu/pdo/PDO.latest"

# get the html pre element text
pdo1 <- html(URL_pdo)
pdo_content <- pdo1 %>% 
               html_node("pre") %>%
               html_text()













# loop through the lines until finding the line starting with "YEAR" and get the column headers
pdo_lines <- unlist(strsplit(pdo_content, split="\n"))  ## split the string at the new lines
i=1
header_line <- strsplit(enso_lines[i], split="\t")
while (grepl("YEAR", header_line) == F) {header_line <- strsplit(enso_lines[i],split="+")
                                         i=i+1
                                         }
# loop through the table to gather the data
new_line <- strsplit(enso_lines[i], split="\t")
data_lines <- unlist(strsplit(enso_lines[i], split="\t"))
while (grepl("2015", new_line) == F) {new_line <- strsplit(enso_lines[i+1], split="\t")
                                      data_lines <- rbind(data_lines, unlist(new_line))
                                      i=i+1
                                      }

enso <- rbind.data.frame(data_lines) # create the dataframe
names(enso) <- matrix(header_line[[1]]) # assign the column name
head(enso)
#







###############################################################################################
###  Water Temperature: 
URL_T <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.31.1"
TGet <- GET(URL_T)
T1 <- content(TGet, as='text')
Tmps <- read.csv(file=textConnection(T1),stringsAsFactors=FALSE)
head(Tmps)

URL_Ts <- "http://gulfwatch.nceas.ucsb.edu/goa/d1/mn/v1/object/df35b.32.1"
TsGet <- GET(URL_Ts)
Ts1 <- content(TsGet, as='text')
TmpSams <- read.csv(file=textConnection(Ts1),stringsAsFactors=FALSE)
head(TmpSams)

















