###########################################################
##### Data Cleaning Script - DMX Linkages
##### Salmon data from ADF&G: Pink and King catch data
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

### Salmon data from ADF&G: http://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisherySalmon.exvesselquery
## orig added by Steph Zador

post_url <- "http://www.adfg.alaska.gov/index.cfm?adfg=CommercialByFisherySalmon.exvesselquery"
post_body <- list(Year="2014", areas= "allareas", speciesx= "allspecies", submit= "Find%20the%20Data")
response <- POST(post_url, body = post_body, encode = "form")
## Use as = "text" here so the response stays a character string because htmlParse expects text
the_content <- content(response, as="text") 
the_html <- htmlParse(the_content)
akcommdf_2014 <- readHTMLTable(the_html)[[1]]


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
            mutate(area=rep(c('southeast','Prince William Sound','Cook Inlet','kodiak',
                              'chignik','AK Peninsula / Aleutian Is.'),21))

acc=1
for(i in 1:nrow(selectionDf)) {
  post_body <- list(Year=selectionDf[i,1], areas=selectionDf[i,2], speciesx='allspecies', submit= "Find%20the%20Data")
  response <- POST(post_url, body = post_body, encode = "form")
  the_content <- content(response, as="text") 
  the_html <- htmlParse(the_content)
  akSalmonDf <- as.data.frame(readHTMLTable(the_html)[[1]])
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
######### King/Chinkook salmon catch data #########

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


