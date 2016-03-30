# Communities of Practice 
# Structural Equation Modelling Script
# Halibut case study
# Colette Ward 29 March 2016
################################################################


library(plyr)
library(dplyr)
library(lavaan)
library(AICcmodavg)
library(psych)
library(zoo)

# call the data assembly script
#source("commPracticeDataFormat.R")
#CPrD <- CoPrct

# or load the data from local source
CPrD <- read.csv("CoPrct.csv", header=T)
head(CPrD)


# log-transform and rename some variables:
CPrHlbt <- CPrD %>%
  mutate(logPinkShrimp = log(Pink_Shrimp), 
         logPlckAdults = log(Poll_Yr3plus_TtlBmss_1000Tons),
         logCrab = log(TotTCrab), 
         logPinkShrimp = log(Pink_Shrimp),
         logHlbt = log(Hlbt_GoAExploitable_lbs), 
         logHlbtPounds = log(hlbt_pounds)) %>%
  rename(#WindSpAn = WndSp_m_s_AnnMn, #eliminate; we don't have data for all years
         ENSO = ENSO_anul_mn, 
         NPGO = NPGO_anul_mn,
         SharkAbund = SharkAbundIPHC) %>%
  select(Year, ENSO, NPGO, logCrab, logPlckAdults, logPinkShrimp, SharkAbund, 
         logHlbt, logHlbtPounds, hlbt_real_rev, hlbt_vessels, hlbt_processors)
names(CPrHlbt)

# look at correlations among Halibut variables
# pairs.panels(CPrHlbt[,c(2:12)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
# Hlbt revenue, vessels, processors are correlated


# lag data to age-0 and age-1 years (calculate moving averages of 
# variables which influence age-0 and age-1 Halibut, 
# lagged to correspond to the largest contributions to the age distribution (ages-7 to 16))
CPrHlbtLags = CPrHlbt %>%
  mutate(ENSO.lag7 = lag(ENSO, n = 7),
         ENSO.lag7to16 = rollapply(data = ENSO.lag7, width = 10, FUN = mean, 
                                   align = "right", fill = NA, na.rm = T), # find mean of ENSO over lags 7-16
         
         NPGO.lag7 = lag(NPGO, n = 7),
         NPGO.lag7to16 = rollapply(data = NPGO.lag7, width = 10, FUN = mean, 
                                   align = "right", fill = NA, na.rm = T), # find mean of NPGO over lags 7-16
         
         # note that crab data begin in 1989, and first year for which lag-6 data exist is 1995; 
         # therefore early years of halibut model (beginning 1996)
         #average over only a few years of crab data (OK if we assume crab biomass was stable in mid-1980s?)
         logCrab.lag6 = lag(logCrab, n = 6),  
         logCrab.lag6to15 = rollapply(data = logCrab.lag6, width = 10, FUN = mean, 
                                      align = "right", fill = NA, na.rm = T), # find mean of logCrab over lags 6-15
         
         logPinkShrimp.lag6 = lag(logPinkShrimp, n = 6),
         logPinkShrimp.lag6to15 = rollapply(data = logPinkShrimp.lag6, width = 10, FUN = mean, 
                                            align = "right", fill = NA, na.rm = T), # find mean of logPinkShrimp over lags 6-15
         
         # Cannot use shark data, time series begins in 1998, therefore there is insufficient data to create lagged time series
         #Sharks.lag6 = lag(SharkAbund, n = 6),
         #Sharks.lag6to15 = rollapply(data = Sharks.lag6, width = 10, FUN = mean, 
                                     #align = "right", fill = NA, na.rm = T), # find mean of Shark Abundance over lags 6-15
  
         logHlbt.lead1 = lead(logHlbt, n = 1)) %>% # create autoregressive term (lag -1) for Halibut biomass
  
         select(Year, ENSO.lag7to16, NPGO.lag7to16, logCrab, logCrab.lag6to15, 
                logPlckAdults, logPinkShrimp.lag6to15, #Sharks.lag6to15, 
                logHlbt, logHlbt.lead1, logHlbtPounds, hlbt_real_rev, hlbt_vessels, hlbt_processors)

View(CPrHlbtLags)
rm(CPrHlbtLags)

# Important: data do not exist to calculate lagged variables before 1991 (ie first year for which lag 7to16 data can be used is 1991)



# select 1996 - 2013
CPrHlbtLags1 <- CPrHlbtLags %>%
  filter(Year > 1995 & Year < 2014)


# look at correlations
pairs.panels(CPrHlbtLags1 [,c(2:12)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
# significant correlations (for variables in the same regressions within the model):
# ENSO.lag7to16, NPGO.lag7to16
# logPinkShrimp.lag6to15, logCrab.lag6to15
# Hlbt revenue, vessels, processors


# standardize each variable to zero mean and unit variance
CPrHlbtLags2 <- CPrHlbtLags1 %>% colwise(scale)()



#############################################
# 1. Halibut base model 
# see slide at: https://docs.google.com/a/noaa.gov/presentation/d/1GPMfcLRIXkg1ZEndSmMcdMFaa3IkU6Lbbs7QabwFewU/edit?usp=sharing
#############################################

ENSO, lagged to age-0 year: for adult year t, take mean of data in years t-16 to t-7 
NPGO, lagged to age-0 year 
logCrab, lagged to juveniles (age-1 year; for adult year t, take mean of data in years t-15 to t-6). no lag for adults
logPlckAdults
logPinkShrimp, lagged to juveniles
SharkAbund, lagged to juveniles    
logHlbt 
autoregressive process for Halibut (lag-1)
logHlbtPounds
hlbt_real_rev
hlbt_vessels
hlbt_processors


mod.1 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logCrab.lag6to15 + logPinkShrimp.lag6to15 +  # variables influencing age-0 and age-1 Halibut
logCrab + logPlckAdults # variables influencing adult Halibut 

'
mod.1.fit <- sem(mod.1, data=CPrHlbtLags2)
summary(mod.1.fit, stand=T, rsq=T)
# lavaan (0.5-20) converged normally after  35 iterations
# Number of observations                            18
# Estimator                                         ML
# Minimum Function Test Statistic               33.885
# Degrees of freedom                                 5
# P-value (Chi-square)                           0.000

# adult Halibut ~ adult pollock is a poor fit (p = 0.71)
# therefore remove it from next model




mod.2 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logCrab.lag6to15 + logPinkShrimp.lag6to15 +  # variables influencing age-0 and age-1 Halibut
logCrab # variables influencing adult Halibut 

'
mod.2.fit <- sem(mod.2, data=CPrHlbtLags2)
summary(mod.2.fit, stand=T, rsq=T)

# model fit is still poor, although most regressions fit well except logCrab:
# so far, suggesting that juvenile survival is more important than adult survival

# lavaan (0.5-20) converged normally after  22 iterations
# Number of observations                            18
# Estimator                                         ML
# Minimum Function Test Statistic               26.745
# Degrees of freedom                                 4
# P-value (Chi-square)                           0.000



# add autoregressive process (lag-1) for Halibut
mod.3 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logCrab.lag6to15 + logPinkShrimp.lag6to15 +  # variables influencing age-0 and age-1 Halibut
logCrab + logPlckAdults + # variables influencing adult Halibut 
logHlbt.lead1 # autoregressive process

'
mod.3.fit <- sem(mod.3, data=CPrHlbtLags2)
summary(mod.3.fit, stand=T, rsq=T)
# still a bad model fit
# lavaan (0.5-20) converged normally after  71 iterations
# Number of observations                            18
# Estimator                                         ML
# Minimum Function Test Statistic               41.543
# Degrees of freedom                                 6
# P-value (Chi-square)                           0.000

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logHlbtPounds ~                                                       
#   logHlbt           0.422    0.214    1.973    0.048    0.422    0.422
# logHlbt ~                                                             
#   NPGO.lag7to16     0.027    0.044    0.618    0.537    0.027    0.027
#   logCrab.lg6t15   -0.000    0.055   -0.003    0.998   -0.000   -0.000
#   lgPnkShrmp.615   -0.211    0.074   -2.852    0.004   -0.211   -0.211
#   logCrab           0.049    0.027    1.822    0.068    0.049    0.049
#   logPlckAdults     0.017    0.029    0.576    0.564    0.017    0.017
#   logHlbt.lead1     0.863    0.067   12.808    0.000    0.863    0.863

# Variances:
#                Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logHlbtPounds     0.776    0.259    3.000    0.003    0.776    0.822
# logHlbt           0.004    0.001    3.000    0.003    0.004    0.004

# R-Square:
#                Estimate
# logHlbtPounds     0.178
# logHlbt           0.996