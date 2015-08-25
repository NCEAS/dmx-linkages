################################################################
## Communities of Place data compilation
## created 24 Aug 2015
## This will replace the spreadsheet started on Google Drive.
################################################################

## load packages
library(dplyr)
library(httr)

## create empty data frame with Year column
CoPlc=data.frame('Year'=c(1975:2015))

### bring in data, format to annual estimates (2 col df with cols=year,spEstimate) and merge with CoPlc dataframe
# CoPlc=merge(CoPlc,newData,all.x=T) ## rename new df CoPlc