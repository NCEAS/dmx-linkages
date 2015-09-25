###########################################################
##### Data Cleaning Script - DMX Linkages
##### Pacific Decadal Oscillation Index (PDO)
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



