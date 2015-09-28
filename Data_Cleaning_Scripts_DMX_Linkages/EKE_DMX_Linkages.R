###########################################################
##### Data Cleaning Script - DMX Linkages
##### Eddy Kinetic Energy (EKE)
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
#head(EKE_df)
#
EKE <- EKE_df %>%
       rename(Region_D=Region.d, Region_C=Region.c, Region_B=Region.b) %>%
       gather(Region, EKE_ann_max, -Year) %>% # reshapes data to be column-wise
       group_by(Year) %>%
       summarize(EKE_ann_max_mean=mean(EKE_ann_max, na.rm=TRUE)) %>% # get annual means
       ungroup()
#



