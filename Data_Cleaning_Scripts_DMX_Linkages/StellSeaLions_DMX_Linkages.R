###########################################################
##### Data Cleaning Script - DMX Linkages
##### Stellar Sea Lions:
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
