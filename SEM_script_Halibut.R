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
         logHlbt, logHlbtPounds, hlbt_real_rev, hlbt_vessels, hlbt_processors, HlbtQuota_2C3A3B)
names(CPrHlbt)

# look at correlations among Halibut variables
# pairs.panels(CPrHlbt[,c(2:13)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
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
         
         Sharks.lag6 = lag(SharkAbund, n = 6),
         Sharks.lag6to15 = rollapply(data = Sharks.lag6, width = 10, FUN = mean, 
                                     align = "right", fill = NA, na.rm = T), # find mean of Shark Abundance over lags 6-15
  
         logHlbt.lead1 = lead(logHlbt, n = 1)) %>% # create autoregressive term (lag -1) for Halibut biomass
  
         select(Year, ENSO.lag7to16, NPGO.lag7to16, logCrab, logCrab.lag6to15, 
                logPlckAdults, logPinkShrimp.lag6to15, Sharks.lag6to15, 
                logHlbt, logHlbt.lead1, logHlbtPounds, hlbt_real_rev, hlbt_vessels, hlbt_processors, HlbtQuota_2C3A3B)

# Important: data do not exist to calculate lagged variables before 1991 (ie first year for which lag 7to16 data can be used is 1991)


# select 1996 - 2013
CPrHlbtLags1 <- CPrHlbtLags %>%
  filter(Year > 1995 & Year < 2014 %>%
  select(-Sharks.lag6to15) %>% # Cannot use shark data for 1996-2013 analyses because time series begins in 1998, therefore there is insufficient data to create lagged time series
  select(-HlbtQuota_2C3A3B) # quota system began in 2006  



# look at correlations
pairs.panels(CPrHlbtLags1 [,c(2:14)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
# significant correlations (for variables in the same regressions within the model):
# ENSO.lag7to16, NPGO.lag7to16
# logPinkShrimp.lag6to15, logCrab.lag6to15
# Hlbt vessels, processors
# quota correlates with all of HlbtPounds, revenue, vessels, processors
# quota correlates strongly with all variables ...

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

logHlbt ~ NPGO.lag7to16 + logCrab.lag6to15 + logPinkShrimp.lag6to15 +     # variables influencing age-0 and age-1 Halibut
logCrab + logPlckAdults                                                   # variables influencing adult Halibut 

'
mod.1.fit <- sem(mod.1, data=CPrHlbtLags2)
summary(mod.1.fit, stand=T, rsq=T)
# poor model fit

# adult Halibut ~ adult pollock is a poor fit (p = 0.71)
# therefore remove it from next model





mod.2 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logCrab.lag6to15 + logPinkShrimp.lag6to15 +    # variables influencing age-0 and age-1 Halibut
logCrab                                                                  # variables influencing adult Halibut 

'
mod.2.fit <- sem(mod.2, data=CPrHlbtLags2)
summary(mod.2.fit, stand=T, rsq=T)

# model fit is still poor, although most regressions fit well except logCrab:
# so far, suggesting that juvenile survival is more important than adult survival




# add autoregressive process (lag-1) for Halibut
mod.3 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logCrab.lag6to15 + logPinkShrimp.lag6to15 +    # variables influencing age-0 and age-1 Halibut
logCrab + logPlckAdults +                                                # variables influencing adult Halibut 
logHlbt.lead1                                                            # autoregressive process

'
mod.3.fit <- sem(mod.3, data=CPrHlbtLags2)
summary(mod.3.fit, stand=T, rsq=T)
# still a bad model fit
modindices(mod.3.fit)
# note large mod indices for logHlbtPounds ~ lagged NPGO, lagged crab, lagged shrimp, & AR term for halibut




# remove logHlbt ~ lagged Crab
mod.4 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logPinkShrimp.lag6to15 +       # variables influencing age-0 and age-1 Halibut
logCrab + logPlckAdults +                                # variables influencing adult Halibut 
logHlbt.lead1                                            # autoregressive process

'
mod.4.fit <- sem(mod.4, data=CPrHlbtLags2)
summary(mod.4.fit, stand=T, rsq=T)

# still a poor fit, although min. test statistic is improved




# remove logHlbt ~ Adult pollock
mod.5 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ NPGO.lag7to16 + logPinkShrimp.lag6to15 +       # variables influencing age-0 and age-1 Halibut
logCrab +                                                # variables influencing adult Halibut 
logHlbt.lead1                                            # autoregressive process

'
mod.5.fit <- sem(mod.5, data=CPrHlbtLags2)
summary(mod.5.fit, stand=T, rsq=T)

# still a poor fit, although min. test statistic is improved




# remove logHlbt ~ lagged NPGO
mod.6 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ logPinkShrimp.lag6to15 +              # variables influencing age-0 and age-1 Halibut
logCrab +                                       # variables influencing adult Halibut 
logHlbt.lead1                                   # autoregressive process

'
mod.6.fit <- sem(mod.6, data=CPrHlbtLags2)
summary(mod.6.fit, stand=T, rsq=T)
# still a poor fit; improvement in min. test statistic is <2 (ie not meaningful)




# remove logHlbt ~ logCrab
mod.7 <- 'logHlbtPounds ~ logHlbt

logHlbt ~ logPinkShrimp.lag6to15 +              # variables influencing age-0 and age-1 Halibut
                                                # variables influencing adult Halibut 
logHlbt.lead1                                   # autoregressive process

'
mod.7.fit <- sem(mod.7, data=CPrHlbtLags2)
summary(mod.7.fit, stand=T, rsq=T)


# still very poor model fit, even though p values are significant for all regressions ...
# no meaningful improvement in min test statistic relative to mod.6
# and shrimp term should be positive, not negative ... lagged shrimp and adult halibut are strongly negatively correlated

# lavaan (0.5-20) converged normally after  44 iterations
# Number of observations                            18
# Estimator                                         ML
# Minimum Function Test Statistic               17.598
# Degrees of freedom                                 2
# P-value (Chi-square)                           0.000

# Parameter Estimates:
# Information                                 Expected
# Standard Errors                             Standard

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logHlbtPounds ~                                                       
#   logHlbt           0.422    0.214    1.973    0.048    0.422    0.422
# logHlbt ~                                                             
#   lgPnkShrmp.615   -0.194    0.030   -6.485    0.000   -0.194   -0.194
#   logHlbt.lead1     0.828    0.030   27.647    0.000    0.828    0.828

# Variances:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
#   logHlbtPounds     0.776    0.259    3.000    0.003    0.776    0.822
#   logHlbt           0.004    0.001    3.000    0.003    0.004    0.004

# R-Square:
#                Estimate
# logHlbtPounds     0.178
# logHlbt           0.996




# look at just the Adult Halibut component:
summary(lm(CPrHlbtLags2$logHlbt ~ CPrHlbtLags2$logPinkShrimp.lag6to15 + CPrHlbtLags2$logHlbt.lead1))



######################################################################
######################################################################
######################################################################

# add Halibut Commercial Catch Limit to model
# there was no quota system before 2006; therefore restrict model to 2006 - 2013


# select 2006 - 2013
CPrHlbtLags3 <- CPrHlbtLags %>%
  filter(Year > 2005 & Year < 2014)
         
# standardize each variable to zero mean and unit variance
CPrHlbtLags4 <- CPrHlbtLags3 %>% colwise(scale)()




# Base model:
mod.8 <- 'logHlbtPounds ~ logHlbt + HlbtQuota_2C3A3B

HlbtQuota_2C3A3B ~ logHlbt.lead1                                         # Quota is set by standing biomass in year t-1

logHlbt ~ logPinkShrimp.lag6to15 +                                       # variables influencing age-0 and age-1 Halibut
logHlbt.lead1                                                            # autoregressive process

'
mod.8.fit <- sem(mod.8, data=CPrHlbtLags4)
summary(mod.8.fit, stand=T, rsq=T)

# model min test statistic is improved, however p-val is still not significant, despite highly significant p values and high R^2 for each regression parameter
# suggests another link (pathway) is still missing
# why is halibut harvest negatively related to halibut biomass?

# lavaan (0.5-20) converged normally after  46 iterations
# Number of observations                             8
# Estimator                                         ML
# Minimum Function Test Statistic               12.940
# Degrees of freedom                                 4
# P-value (Chi-square)                           0.012

# Regressions:
#                    Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logHlbtPounds ~                                                         
#   logHlbt            -1.123    0.113   -9.966    0.000   -1.123   -0.939
#   HlbtQut_2C3A3B      1.939    0.113   17.207    0.000    1.939    1.622
# HlbtQuota_2C3A3B ~                                                      
#   logHlbt.lead1       0.854    0.184    4.637    0.000    0.854    0.854
# logHlbt ~                                                               
#   lgPnkShrmp.615     -0.463    0.130   -3.549    0.000   -0.463   -0.463
#   logHlbt.lead1       0.546    0.130    4.192    0.000    0.546    0.546

# Variances:
#                Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logHlbtPounds     0.027    0.014    2.000    0.046    0.027    0.022
# HlbtQut_2C3A3B    0.237    0.119    2.000    0.046    0.237    0.271
# logHlbt           0.017    0.009    2.000    0.046    0.017    0.020

# R-Square:
#                Estimate
# logHlbtPounds     0.978
# HlbtQut_2C3A3B    0.729
# logHlbt           0.980