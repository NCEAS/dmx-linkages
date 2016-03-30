###########################################################
##### Data Cleaning Script - DMX Linkages
##### Halibut Biomass
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
# Halibut biomass estimates
# from 2015 IPHC RARA  (URL here)

# population biomass data are from Table 2 of the Stock Assessment chapter (2015 IPHC RARA Halibut stock assessment).
# we will use Median Exploitable Biomass, units are millions of lbs. (NB Median Spawning Biomass is also available):
URL_Hlbt <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uMDl0MWM1UFdnVlk"
HlbtGet <- GET(URL_Hlbt)
Hlbt1 <- content(HlbtGet, as='text')
Hlbt_df <- read.csv(file=textConnection(Hlbt1),stringsAsFactors=FALSE)
# or load data from local source:
#Hlbt_df <- read.csv("Halibut_2015SA_Table2.csv", header=T)

HlbtPop_df1 <- Hlbt_df %>%
  mutate(HlbtExploitable_RndWt_Mlbs = ExploitableBiomass/0.75) %>% # reported weights are 75% of round weight (ie head & guts removed); this line scales our data to 100% round weight
  mutate(HlbtExploitable_RndWt_lbs = HlbtExploitable_RndWt_Mlbs*1000000) %>% # this line scales our data to lbs
  select(Year, HlbtExploitable_RndWt_lbs)





# Use Setline survey weight per unit effort to apportion population biomass (> 32 inches) by geographic region:
# these are from Table 1 of the Apportionment chapter of the 2015 IPHC RARA Halibut stock assessment
URL_HlbtApp <- "https://drive.google.com/uc?export=download&id=0B1XbkXxdfD7uZFVsNHIyX3JuRlE"
HlbtAppGet <- GET(URL_HlbtApp)
HlbtApp1 <- content(HlbtAppGet, as='text')
HlbtApp_df <- read.csv(file=textConnection(HlbtApp1),stringsAsFactors=FALSE)
# or load data from local source:
#HlbtApp_df <- read.csv("Halibut_2015Apportionment.csv", header=T)

HlbtApp_df1 <- HlbtApp_df %>%
  mutate(GoA_App = X3A + X3B + X2C) %>% # we want areas 3A, 3B, and possibly 2C (Southeast AK)?
  transmute(Year, GoA_App = GoA_App/100)

# there are no geographic apportionment estimates for 1996-1999. use mean apportionment over 2000-2016 (65.12%)
HlbtApp_df2 <- data.frame(matrix(NA_real_, nrow = 4, ncol = 2))
HlbtCols <- c("Year", "GoA_App"); colnames(HlbtApp_df2) <- HlbtCols
HlbtApp_df2$Year <- seq(1996, 1999, 1)
HlbtApp_df2$GoA_App <- paste(mean(HlbtApp_df1$GoA_App)) # use mean apportionment over 2000-2016 (65.12%)

# merge tables of 1996-1999 assumption and 2000-2016 estimates:
HlbtApp_df3 <- data.frame(matrix(NA_real_, nrow = 21, ncol = 2))
HlbtCols <- c("Year", "GoA_App"); colnames(HlbtApp_df3) <- HlbtCols
HlbtApp_df3$Year <- seq(1996, 2016, 1)
HlbtApp_df4 <- left_join(HlbtApp_df3, HlbtApp_df2, by = "Year") %>% # merge in 1996-1999 apportionment assumption
  transmute(Year, GoA_App = ifelse(is.na(GoA_App.x), GoA_App.y, GoA_App.x))
HlbtApp_df4 <- left_join(HlbtApp_df4, HlbtApp_df1, by = "Year") %>% # merge in 2000-2016 apportionment estimates
  transmute(Year, GoA_App = ifelse(is.na(GoA_App.x), GoA_App.y, GoA_App.x)) 





HlbtBiomass_df <- left_join(HlbtPop_df1, HlbtApp_df4, by = "Year") %>% # merge population and apportionment dataframes
  transmute(Year, Hlbt_GoAExploitable_lbs = HlbtExploitable_RndWt_lbs*as.numeric(GoA_App)) # scale exploitable biomass for geographic apportionment
#View(HlbtBiomass_df)