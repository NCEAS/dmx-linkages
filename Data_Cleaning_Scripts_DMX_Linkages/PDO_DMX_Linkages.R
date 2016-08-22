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
pdo_raw <- read_html(URL_pdo)
pdo_pre <- pdo_raw %>%
               html_node("p") %>%
               html_text()
pdo_cols <- scan(textConnection(pdo_pre), skip=29, nlines=1, what=character())# Get header row
pdo_df <- read.table(file=textConnection(pdo_pre), skip=30, nrows=116, stringsAsFactors=F, sep="",
                  header=FALSE, col.names=pdo_cols, strip.white=TRUE, fill=TRUE)
pdo_df$YEAR <- substr(pdo_df$YEAR, 1, 4)  # removes asterisks from years 2002-2015

pdo_pre2 <- pdo_df %>%
              rename(Year=YEAR) %>% # rename data columns
              filter(Year %in% c(1974:2015)) %>% # selects years 1974 - 2015
              select(Year, JAN, FEB, DEC) %>% # select only Dec, Jan, Feb data
              gather(Month, PDO, -Year) %>% # reshapes data to be column-wise
              mutate(Year = as.integer(Year))
  
for(i in 1:nrow(pdo_pre2)) {
  if(pdo_pre2$Month[i] == "DEC") {pdo_pre2$Year[i] <- pdo_pre2$Year[i]+1} # assign year t+1 to each Dec estimate, so that sequential Dec, Jan, Feb estimates are grouped
}

pdo_winter <- pdo_pre2 %>%
  filter(Year != 1974, Year != 2016) %>% # remove year 1974 now that 1974 data for December has been assigned to the 1975 group
  group_by(Year) %>%
  summarise(PDO_winter = mean(as.numeric(as.character(PDO)), na.rm = TRUE)) %>% # get winter means
  ungroup()
#



