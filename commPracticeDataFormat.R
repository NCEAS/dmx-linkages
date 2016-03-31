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
## 1) run each data cleaning script to generate data frames
## 2) create empty data frame
## 3) merge all data frames with CoPrct dataframe:   
##        CoPrct=merge(CoPrct,newData,all.x=T)  


# Source and run each data cleaning script
sourceDir <- function(path, trace=TRUE) {
    for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
       if(trace) cat(nm,":")
       source(file.path(path, nm))
       if(trace) cat("\n")
    }
}

sourceDir("Data_Cleaning_Scripts_DMX_Linkages")


# Create empty data frame with Year column
CoPrct <- data.frame('Year'=c(1975:2015))


# Merge all data frames into one large data frame
CoPrct <- merge(CoPrct,NPI,all.x=T)                  # North Pacific Index of sea level pressure
CoPrct <- merge(CoPrct,Wind_Ann,all.x=T)             # Annual mean wind - Central GOA
CoPrct <- merge(CoPrct,Wind_Winter,all.x=T)          # Winter mean wind - Central GOA
CoPrct <- merge(CoPrct,ENSO_annual,all.x=T)          # ENSO annual index
CoPrct <- merge(CoPrct,npgo_annual,all.x=T)          # North Pacific Gyre Oscillation
CoPrct <- merge(CoPrct,pdo_annual,all.x=T)           # Pacific Decadal Oscillation
CoPrct <- merge(CoPrct,upanom,all.x=T)               # Pacific Upwelling Anomalies
CoPrct <- merge(CoPrct,PC_df,all.x=T)                # Pacific Cod Stock Assessment
CoPrct <- merge(CoPrct,EKE,all.x=T)                  # Eddy Kinetic Energy - Central GOA
CoPrct <- merge(CoPrct,TCrab,all.x=T)                # Tanner Crab Abundance 
CoPrct <- merge(CoPrct,SST,all.x=T)                  # Water Temperatures from Seward Line CTD
CoPrct <- merge(CoPrct, Poll_Adult,all.x=T)          # Pollock Adults
CoPrct <- merge(CoPrct, Poll_Yr1,all.x=T)            # Pollock Recruits (age-1)
CoPrct <- merge(CoPrct, Poll_FemaleSpawning,all.x=T) # Pollock Female Spawning Biomass
CoPrct <- merge(CoPrct,Arr_df,all.x=T)               # Arrowtooth adult biomass 
CoPrct <- merge(CoPrct,ArrFishery_df,all.x=T)        # Arrowtooth fishery data
CoPrct <- merge(CoPrct,WPinks,all.x=T)               # PWS pink salmon SSB
CoPrct <- merge(CoPrct,pinkDf,all.x=T)               # Pink salmon catch data
CoPrct <- merge(CoPrct,kingDf,all.x=T)               # King salmon catch data
CoPrct <- merge(CoPrct,Shr_df,all.x=T)               # Pink shrimp 
CoPrct <- merge(CoPrct,ShSk_df,all.x=T)              # Sharks and Skates
CoPrct <- merge(CoPrct,SSL,all.x=T)                  # Stellar Sea Lions
CoPrct <- merge(CoPrct,AnnChl,all.x=T)               # log Chla from satellites
CoPrct <- merge(CoPrct,HlbtFishery_df,all.x=T)       # Halibut catch data from fishery
CoPrct <- merge(CoPrct,PollFishery_df,all.x=T)       # Pollock catch data from fishery
CoPrct <- merge(CoPrct,PollTAC_df,all.x=T)           # Pollock quota (TAC)
CoPrct <- merge(CoPrct,SewardLineMayCopepods,all.x=T)# Copepod biomass - May (Seward Line)
CoPrct <- merge(CoPrct,Arr_df,all.x=T)               # Arrowtooth adult biomass 
CoPrct <- merge(CoPrct,HlbtBiomass_df,all.x=T)       # Halibut exploitable biomass (lbs)
CoPrct <- merge(CoPrct,ArrFishery_df,all.x=T)        # Arrowtooth fishery data
CoPrct <- merge(CoPrct,WPinks,all.x=T)               # PWS pink salmon SSB
CoPrct <- merge(CoPrct,pinkDf,all.x=T)               # Pink salmon catch data
CoPrct <- merge(CoPrct,kingDf,all.x=T)               # King salmon catch data
CoPrct <- merge(CoPrct,Shr_df,all.x=T)               # Pink shrimp 
CoPrct <- merge(CoPrct,ShSk_df,all.x=T)              # Sharks and Skates
CoPrct <- merge(CoPrct,SSL,all.x=T)                  # Stellar Sea Lions
CoPrct <- merge(CoPrct,AnnChl,all.x=T)               # log Chla from satellites
CoPrct <- merge(CoPrct,HlbtFishery_df,all.x=T)       # Halibut catch data from fishery
CoPrct <- merge(CoPrct,PollFishery_df,all.x=T)       # Pollock catch data from fishery
CoPrct <- merge(CoPrct,PollTAC_df,all.x=T)           # Pollock quota (TAC)
CoPrct <- merge(CoPrct,SewardLineMayCopepods,all.x=T)# Copepod biomass - May (Seward Line)
CoPrct <- merge(CoPrct,SewardLineMayEuphausiids,all.x=T)# Euphausid biomass - May (Seward Line)
CoPrct <- merge(CoPrct,Line8Zoop,all.x=T)               # Large copepod, euphausiid, large other zooplankton abundances (Line 8, ie Shelikof Strait)
CoPrct <- merge(CoPrct,Cap_df,all.x=T)               # Capelin Index
CoPrct <- merge(CoPrct,HlbtQuota_df1,all.x=T)        # Halibut commercial catch limits


# Optional: Write data frame to a CSV
#write.csv(CoPrct, file = "CoPrct.csv", row.names=FALSE)


###############################################################################################

