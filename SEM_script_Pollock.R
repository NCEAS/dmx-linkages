# Communities of Practice 
# Structural Equation Modelling Script
# Pollock case study
# Colette Ward March 2016
################################################################


library(plyr)
library(dplyr)
library(car)
library(zoo)
library(psych)
library(lavaan)
library(AICcmodavg)

# call the data assembly script
#source("commPracticeDataFormat.R")
#CPrD <- CoPrct

# or load the data from local source
CPrD <- read.csv("CoPrct.csv", header=T)
head(CPrD)


# log-transform and rename some variables:
CPrPlck <- CPrD %>%
  mutate(logEuphausiids = log(SewardLineMayEuphausiids),
         logCopepods = log(SewardLineMayCopepods),
         logPinkShrimp = log(Pink_Shrimp),
         logPlckRecruits = log(Poll_Age1_recruits_millions*1000000),
         logPlckAdults = log(Poll_Yr3plus_TtlBmss_1000Tons*1000),
         logPlckSSB = log(Poll_FemaleSpawningBmss_1000Tons*1000),
         logArrAdult = log(ArrAdult),
         logPCodFem = log(PCod_female_Bmss_t),
         logHlbt = log(Hlbt_GoAExploitable_lbs),
         logPlckTons = log(plck_tons),
         logPlckVessels = log(plck_vessels),
         logPlckTAC = log(PollockTAC_tons)) %>%
  rename(NPGO = NPGO_anul_mn,
         PDO = PDO_anul_mn, 
         WTemp = WTemp_C_AnnMn, 
         logAnnChl = AnnChl,
         PlckPrice = plck_real_price_SAFE) %>%
  select(Year, NPGO, PDO, WTemp, logAnnChl, logEuphausiids, logCopepods, logPinkShrimp, 
         logPlckRecruits, logPlckAdults, logPlckSSB, logArrAdult, logPCodFem, logHlbt, 
         logPlckTons, logPlckVessels, PlckPrice, logPlckTAC)
names(CPrPlck)


# lag some time series:
CPrPlckLags = CPrPlck %>%
  mutate(logEuphausiids.lag1 = lag(logEuphausiids, n = 1), # lag Euphausiid data to year t-1, because these are predominantly Furcillia, not adults
         logPlckSSB.lag1 = lag(logPlckSSB, n = 1), # lag Female SSB to year t-1
         logPlckAdults.lead1 = lead(logPlckAdults, n = 1)) %>% # create autoregressive term (lag-1) for Adult Pollock biomass
  select(-logPlckSSB)


#############################################
# Fit and calculate residuals of stock-recruit relationship 
# (using data for all years in the dataframe)
#############################################

recruits <- CPrPlckLags$logPlckRecruits
spawners <- CPrPlckLags$logPlckSSB.lag1

# take residuals of log (R/S) ~ log(S)
plot(log(recruits/spawners) ~ log(spawners), pch=16, cex=2.5)
regLine(lm(log(recruits/spawners) ~ log(spawners)), col="red")
sr.model <- lm(log(recruits/spawners) ~ log(spawners))
summary(sr.model)
# Multiple R-squared:  0.4089,	Adjusted R-squared:  0.392 
# F-statistic: 24.21 on 1 and 35 DF,  p-value: 2.042e-05
SRresidPlck <- exp(sr.model$residuals) # create column of raw stock-recruit residuals

# add residuals to dataframe:
srResiduals <- data.frame('Year'=c(1978:2014))
#srResiduals <- bind_cols(srResiduals, as.data.frame(logSRresidPlck)) # merge in log stock-recruit residuals
srResiduals <- bind_cols(srResiduals, as.data.frame(SRresidPlck)) # merge in raw stock-recruit residuals
CPrPlckLags <- merge(CPrPlckLags, srResiduals, all.x=T)

# plot Stock-Recruit Residuals over time:
plot(srResiduals$SRresidPlck ~ srResiduals$Year, pch=16, cex=2.5, type="b", ylab="Residuals of Pollock log(R/S) ~ log(S)", xlab="Year")


# create column of recruit residuals, summed for lags 2-7
# (to give sum of recruit residuals for recruitment years for a given year's ages 3-8, 
# which seems to represent most of the population in any given year)
CPrPlckLags1 <- CPrPlckLags %>%
  mutate(SRresidPlck.lag2 = lag(SRresidPlck, n = 2),
         SRresidPlck.lag2to7 = rollapply(data = SRresidPlck.lag2, width = 6, FUN = sum,  # sum recruitment residuals over lags 2 to 7
                                   align = "right", fill = NA, na.rm = T),
         logPlckRecruitSums = log(SRresidPlck.lag2to7)) %>% # calculate log of summed residuals
  select(-SRresidPlck.lag2, -SRresidPlck.lag2to7, -logPlckSSB.lag1)



#############################################
# 2. Pollock model 
# see slide at: https://docs.google.com/a/noaa.gov/presentation/d/1GPMfcLRIXkg1ZEndSmMcdMFaa3IkU6Lbbs7QabwFewU/edit?usp=sharing
#############################################

# select 1998 - 2010
CPrPlckLags2 <- CPrPlckLags1 %>%
  filter(Year > 1997 & Year < 2011)

# look at correlations among non-fishery variables:
pairs.panels(CPrPlckLags2[,c(2:13,18:20)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)

# look at correlations among pollock fishery variables:
pairs.panels(CPrPlckLags2[,c(10,14:17)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)


# standardize each variable to zero mean and unit variance
CPrPlckLags3 <- CPrPlckLags2 %>% colwise(scale)()






###################################################
###################################################
###################################################

# model after removing nodes for which we have little/no data (Wind, Transport, juvenile growth & abundance)
mod.P1 <- 'logPlckTons ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ NPGO + PDO'
mod.P1.fit <- sem(mod.P1, data=CPrD5) 
summary(mod.P1.fit, stand=T, rsq=T)
# Found more than one class "Model" in cache; using the first, from namespace 'MatrixModels'
# Warning message:
#   In lav_data_full(data = data, group = group, group.label = group.label,  :
#     lavaan WARNING: small number of observations (nobs < nvar)
#     nobs = 13 nvar = 14


# lavaan (0.5-20) converged normally after  27 iterations
# Number of observations                            13
# Estimator                                         ML
# Minimum Function Test Statistic             1022.591
# Degrees of freedom                                51
# P-value (Chi-square)                           0.000


# things to consider: 

# add a driver of shrimp?

# constrain model to negative coefficient for adult arrowtooth & adult p cod predation on adult pollock?
# constrain model to negative coefficient for adult arrowtooth & adult pollock predation on juv pollock?
# how best to include Halibut? (predation on adult pollock). Use logHlbtPounds? (do we know relationship between halibut popn & catch?)

# there is no correlation betweeen adult pollock biomass and any pollock fishery metric
# wondering if the link is via declining quota with declining population?

# get global model for lower food web to use for all case studies (atm forcing, wind, water temp, phyto, copepods, euphausiids)
# parts of Arrowtooth model need pollock, and vice versa. make sure the models use same variables? eventually merge them into one mega model?


# questions:
# - do euphausiids consume copepods or phytoplankton?
# - what do shrimp consume?







# mod.P1: more variables than observations
# removed Water Temp because results of mod.P1 suggest it has little & statistically insignificant effect anywhere
mod.P2 <- 'logPlckTons ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ NPGO + PDO'
mod.P2.fit <- sem(mod.P2, data=CPrD5) 
summary(mod.P2.fit, stand=T, rsq=T)
# model is still a terrible fit
# lavaan (0.5-20) converged normally after  27 iterations
# Number of observations                            13
# Estimator                                         ML
# Minimum Function Test Statistic              634.551
# Degrees of freedom                                40
# P-value (Chi-square)                           0.000


modindices(mod.P2.fit)
# Residual covarations to consider with modificaiton index:
# logPlckAdults ~~     srResidPlckLog 10.227    these covary because adult population and recruitment are both declining long-term
plot(CPrD5$logPlckAdults ~ CPrD5$srResidPlckLog, pch=16, cex=2.5)
cor.test(CPrD5$logPlckAdults, CPrD5$srResidPlckLog) # p = 0.178, r = 0.4
# logCopepods ~~          logAnnChl  5.644


# Direct links to consider, with modification index:
# logPlckTons  ~     srResidPlckLog  4.688
# logPlckTons  ~     logEuphausiids  4.222
# logPlckTons  ~ logPlckRecruitSums  4.263
# logPlckTons  ~        logArrAdult  5.635
# logPlckTons  ~         logPCodFem  7.293
# logPlckAdults  ~     srResidPlckLog  5.957
# srResidPlckLog  ~ logPlckRecruitSums  4.565
# srResidPlckLog  ~    logPlckRecruits  7.357
# logCopepods  ~                PDO  8.176
# logAnnChl  ~        logCopepods  5.644
# logAnnChl  ~        logArrAdult  6.308
# logAnnChl  ~         logPCodFem  6.393
# logArrAdult  ~     logEuphausiids  3.941



# notice that Pollock Harvest is not at all correlated with Adult Pollock Biomass
# therefore added Pollock TAC:


mod.P3 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ NPGO + PDO'

mod.P3.fit <- sem(mod.P3, data=CPrD5) 
summary(mod.P3.fit, stand=T, rsq=T)
# lavaan (0.5-20) converged normally after  42 iterations
# Number of observations                            13
# Estimator                                         ML
# Minimum Function Test Statistic              380.260
# Degrees of freedom                                52
# P-value (Chi-square)                           0.000

# note that pollock harvest is very well explained by pollock TAC - good addition
# however, there are more variables than observations, therefore must prune more variables
# use mod.P3.fit to do so

Regressions:
                 Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
logPlckTons ~                                                         
  logPlckTAC        1.085    0.051   21.394    0.000    1.085    0.953
logPlckTAC ~                                                          
  logPlckAdults    -0.307    0.262   -1.172    0.241   -0.307   -0.309
logPlckAdults ~                                                       
  logPlckRcrtSms    0.392    0.475    0.825    0.410    0.392    0.389
  logPlckRecruts   -0.313    0.353   -0.887    0.375   -0.313   -0.311
  logEuphausiids    0.175    0.222    0.791    0.429    0.175    0.174
  logPinkShrimp    -0.302    0.267   -1.130    0.259   -0.302   -0.299
  logArrAdult      -0.708    1.956   -0.362    0.718   -0.708   -0.702
  logPCodFem       -1.143    2.006   -0.570    0.569   -1.143   -1.135
srResidPlckLog ~                                                      
  logEuphausiids   -0.011    0.126   -0.086    0.931   -0.011   -0.010
  logCopepods      -0.227    0.123   -1.843    0.065   -0.227   -0.213
  logAnnChl         0.420    0.127    3.318    0.001    0.420    0.393
  logPlckAdults     0.008    0.130    0.060    0.952    0.008    0.007
  logArrAdult       0.504    0.128    3.942    0.000    0.504    0.471
logEuphausiids ~                                                      
  logAnnChl         0.062    0.277    0.223    0.824    0.062    0.062
logCopepods ~                                                         
  logAnnChl         0.015    0.277    0.052    0.958    0.015    0.015
logAnnChl ~                                                           
  NPGO              0.563    0.241    2.340    0.019    0.563    0.563
  PDO              -0.184    0.241   -0.763    0.445   -0.184   -0.184

Covariances:
                 Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
logPlckTons ~~                                                        
  srResidPlckLog   -0.237    0.101   -2.349    0.019   -0.237   -0.859

Variances:
               Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
logPlckTons       0.110    0.043    2.550    0.011    0.110    0.092
logPlckTAC        0.836    0.328    2.550    0.011    0.836    0.904
logPlckAdults     0.589    0.231    2.550    0.011    0.589    0.628
srResidPlckLog    0.697    0.273    2.550    0.011    0.697    0.660
logEuphausiids    0.920    0.361    2.550    0.011    0.920    0.996
logCopepods       0.923    0.362    2.550    0.011    0.923    1.000
logAnnChl         0.498    0.195    2.550    0.011    0.498    0.539

R-Square:
               Estimate
logPlckTons       0.908
logPlckTAC        0.096
logPlckAdults     0.372
srResidPlckLog    0.340
logEuphausiids    0.004
logCopepods       0.000
logAnnChl         0.461

# consider pruning:
# very poor fit of logCopepods~logAnnChl (p=0.958), also logEuphausiids~logAnnChl (p=0.824). do mod.indices indicate better direct path from NPGO or PDO to Copepods & Euphausiids without Chl a?  BUT note significant effect of logAnnChl on srResidPlckLog (p=0.001, std est = 0.393)
# very poor fit of srResidPlckLog ~ logPlckAdults (a predation link; p = 0.952). although adults may consume juveniles, their population may be too low to have a meaningful impact, especially relative to other potential drivers.
# very poor fit of srResidPlckLog ~ logEuphausiids (p=0.931). This is supposed to be a stronger link than Copepods, which are only alternative prey for juveniles when Euphausiids are scarce.  Also note that copepod impact on s/r residuals is negative (weird, should be positive ...)
# does everything in the above line suggest that srResidPlckLog is a metric of processes during the age-0 year rather than age-1 (ie the juvenile stage)?

modindices(mod.P3.fit)
#    logCopepods  ~                PDO 8.176
#    logCopepods  ~               NPGO 2.939
# logEuphausiids  ~                PDO 0.474
# logEuphausiids  ~               NPGO 0.316

# also consider covariations:
# logCopepods ~~          logAnnChl 5.644
# logPlckTons ~~      logPlckAdults 7.029
# direct relationship:
# logPlckTons  ~      logPlckAdults 6.439



# remove direct effect of AnnChl on Copepods & Euphausiids; replaced with direct link from NPGO and PDO
# but did not remove AnnChl from model entirely, because there is a significant effect of logAnnChl on srResidPlckLog (p=0.001, std est = 0.393)

mod.P4 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + logPlckAdults + logArrAdult
logEuphausiids ~ NPGO + PDO
logCopepods ~ NPGO + PDO
logAnnChl ~ NPGO + PDO'

mod.P4.fit <- sem(mod.P4, data=CPrD5) 
summary(mod.P4.fit, stand=T, rsq=T)
# still too few observations (more variables than observations)
# lavaan (0.5-20) converged normally after  51 iterations
# Number of observations                            13
# Estimator                                         ML
# Minimum Function Test Statistic              371.429
# Degrees of freedom                                50
# P-value (Chi-square)                           0.000


# PDO has no significant effect on AnnChl, but NPGO does (p=0.019, std est = 0.563)
# NPGO has no significant efect on Copepods, but PDO does
# neither NPGO nor PDO have a significant effect on Euphausiids
# eliminate NPGO altogether from model? check first for any other potential links using mod.indices
# check also

modindices(mod.P4.fit)
# the only modification indices >2 for NPGO as causal link are:
# logPlckTons  ~               NPGO 2.882
# srResidPlckLog  ~               NPGO 2.686
# none are very high
# therefore eliminate NPGO entirely from the model:


# remove NPGO from Copepod regression, and PDO from AnnChl regression, and NPGO from Euphausiids:
mod.P5 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + logPlckAdults + logArrAdult
logEuphausiids ~ PDO
logCopepods ~ PDO
logAnnChl ~ NPGO'

mod.P5.fit <- sem(mod.P5, data=CPrD5) 
summary(mod.P5.fit, stand=T, rsq=T)
# still too many variables
# and no change in model fit


# remove Pollock Adults from srResidPlckLog (p = 0.952, std est = 0.007)
# although adults may consume juveniles, their population may be too low to have a meaningful impact, especially relative to other potential drivers.
mod.P6 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + logArrAdult
logEuphausiids ~ PDO
logCopepods ~ PDO
logAnnChl ~ NPGO'

mod.P6.fit <- sem(mod.P6, data=CPrD5) 
summary(mod.P6.fit, stand=T, rsq=T)

# still too many variables; model fit is unchanged


# We are missing Halibut predation from Adult Pollock model - 2015 Stock Assessment suggests it's a substantial source of mortality (Fig 1.42, p101)
# poor fit of Euphausiids as drivers in Adult Pollock and Juv Pollock regressions, but this is difficult to believe give diet information (see Fig 1.41 p 100 in 2015 Stock Assessment)
# ?remove Pollock Recruits from Adult Pollock regression, because its coefficient is negative (should be positive)? BUT info in Stock Assessment (Fig 1.42, p101) suggests it should be important (11% of juv mortality)
# I'm hesitant to remove any more from the Adult Pollock model without first adding Halibut, because this might change things
# maybe I need mean of spring and fall Euphausiids, instead of just spring Euphausiids?

# Overall: are we missing too many years for causal patterns to be apparent?
# eg from correlation, there should be an effet of Pollock TAC on Pollock Harvest (tons), but there is not in these results ...
# same goes for other variables?????

plot(CPrD4$logPlckAdults ~ CPrD4$Year, pch=16, cex=2.5, type = "b")
plot(CPrD4$logPlckTAC~ CPrD4$Year, pch=16, cex=2.5, type = "b")
# no trend
# population is increasing at the recent end, but has the human system been able to catch up yet? is there a time lag here?




# what happens if I add Halibut Harvest to Adult Pollock model as surrogate of predation?

mod.P7 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults
logPlckAdults ~ logPlckRecruitSums + logPlckRecruits + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem + logHlbtPounds
srResidPlckLog ~ logEuphausiids + logCopepods + logAnnChl + logArrAdult
logEuphausiids ~ PDO
logCopepods ~ PDO
logAnnChl ~ NPGO'

mod.P7.fit <- sem(mod.P7, data=CPrD5) 
summary(mod.P7.fit, stand=T, rsq=T)
# model fit is signficantly worse
# Pollock Adults ~ Halibut Pounds p = 0.822, std est = 0.14





###################################################
###################################################
###################################################

# Repeat analysis with autoregressive (lag-1) term for Adult Pollock:

# model after removing nodes for which we have little/no data (Wind, Transport, juvenile growth & abundance)
mod.Pol1 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + SRresidPlck +                  # note that SRresidPlck is here to represent canibalism (and I use SR residuals to remove autoregressive effect ... however note the inverse equation is in SRresidPlck regression ...)
                logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem + logPlckAdults.lead1
SRresidPlck ~ logEuphausiids + logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ NPGO + PDO'
mod.Pol1.fit <- sem(mod.Pol1, data=CPrPlckLags3) 
summary(mod.Pol1.fit, stand=T, rsq=T)
# warning: more observations than variables

# logPlckAdults ~ SRresidPlck represents canniablism and thereore should be negative, but it's not
# therefore remove this from next model:
mod.Pol2 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logEuphausiids + logPinkShrimp + logArrAdult + logPCodFem + logPlckAdults.lead1
SRresidPlck ~ logEuphausiids + logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ NPGO + PDO'
mod.Pol2.fit <- sem(mod.Pol2, data=CPrPlckLags3) 
summary(mod.Pol2.fit, stand=T, rsq=T)
# warning: more observations than variables


# p-vals are insignificant for (logPlckAdults ~ logPCodFem) and (logPlckAdults ~ logArrAdult)
# remove these:
mod.Pol3 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logEuphausiids + logPinkShrimp + logPlckAdults.lead1
SRresidPlck ~ logEuphausiids + logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ NPGO + PDO'
mod.Pol3.fit <- sem(mod.Pol3, data=CPrPlckLags3) 
summary(mod.Pol3.fit, stand=T, rsq=T)
# warning: more observations than variables


# p-val is insignificant for logPlckAdults ~ logPinkShrimp
# therefore remove this:
mod.Pol4 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logEuphausiids + logPlckAdults.lead1
SRresidPlck ~ logEuphausiids + logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logEuphausiids ~ logAnnChl
logCopepods ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ NPGO + PDO'
mod.Pol4.fit <- sem(mod.Pol4, data=CPrPlckLags3) 
summary(mod.Pol4.fit, stand=T, rsq=T)


# poor p-values suggest Euphausiid pathway is not important
# check that modindices don't suggest another Euphausiid pathway
modindices(mod.Pol4.fit)
# logEuphausiids  ~ logPlckAdults.lead1 2.776 # Euphausiids are furcillia, not adults ... indicates pollock predation on adults in year t-1 sets Euphausiid furcillia in year t?


# remove all Euphausiid pathways from the model:
mod.Pol5 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logPlckAdults.lead1
SRresidPlck ~ logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logCopepods ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ NPGO + PDO'
mod.Pol5.fit <- sem(mod.Pol5, data=CPrPlckLags3) 
summary(mod.Pol5.fit, stand=T, rsq=T)


# given modindices(mod.Pol5.fit) and p-values for mod.4, work with Chl a and WTemp regressions:
# indirect pathway from NPGO to Copepods, via WTemp and Chl a is not significant
# remove logCopepods ~ logAnnChl
# add logCopepods ~ PDO
# remove WTemp ~ PDO (not significant)

mod.Pol5 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logPlckAdults.lead1
SRresidPlck ~ logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logCopepods ~ PDO
logAnnChl ~ WTemp
WTemp ~ NPGO'
mod.Pol5.fit <- sem(mod.Pol5, data=CPrPlckLags3) 
summary(mod.Pol5.fit, stand=T, rsq=T)
 
modindices(mod.Pol5.fit)

# given modindices(mod.Pol5.fit) and p-values for mod.4, work with Chl a and WTemp regressions:
# although WTemp ~ NPGO  is significant, pathway from NPGO to Chl a (logAnnChl ~ WTemp) is not significant
#  logCopepods ~~           logAnnChl 4.016
# logAnnChl  ~                NPGO 5.414
# logAnnChl  ~                 PDO 2.654


mod.Pol6 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logPlckAdults.lead1
SRresidPlck ~ logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logCopepods ~ PDO
logAnnChl ~ NPGO + PDO
WTemp ~ NPGO

'
mod.Pol6.fit <- sem(mod.Pol6, data=CPrPlckLags3) 
summary(mod.Pol6.fit, stand=T, rsq=T)


# logAnnChl ~ PDO is not significant; remove it:
mod.Pol7 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logPlckAdults.lead1
SRresidPlck ~ logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logCopepods ~ PDO
logAnnChl ~ NPGO
WTemp ~ NPGO

'
mod.Pol7.fit <- sem(mod.Pol7, data=CPrPlckLags3) 
summary(mod.Pol7.fit, stand=T, rsq=T)

# lavaan (0.5-20) converged normally after  32 iterations
# Number of observations                            13
# Estimator                                         ML
# Minimum Function Test Statistic              150.354
# Degrees of freedom                                43
# P-value (Chi-square)                           0.000

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logPlckTons ~                                                         
#   logPlckTAC        1.078    0.046   23.283    0.000    1.078    0.953
# logPlckTAC ~                                                          
#   lgPlckAdlts.l1   -0.592    0.224   -2.645    0.008   -0.592   -0.592
# logPlckAdults ~                                                       
#   logPlckRcrtSms    0.339    0.174    1.945    0.052    0.339    0.339
#   lgPlckAdlts.l1    0.778    0.174    4.471    0.000    0.778    0.778
# SRresidPlck ~                                                         
#   logCopepods      -0.203    0.111   -1.831    0.067   -0.203   -0.157
#   logAnnChl         0.654    0.123    5.315    0.000    0.654    0.505
#   WTemp             0.322    0.116    2.784    0.005    0.322    0.249
#   logPlckAdults     0.184    0.122    1.513    0.130    0.184    0.142
#   logArrAdult       0.665    0.117    5.670    0.000    0.665    0.514
# logCopepods ~                                                         
#   PDO               0.687    0.202    3.410    0.001    0.687    0.687
# logAnnChl ~                                                           
#   NPGO              0.661    0.208    3.174    0.002    0.661    0.661
# WTemp ~                                                               
#   NPGO             -0.488    0.242   -2.017    0.044   -0.488   -0.488

# Covariances:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logPlckTons ~~                                                        
#   SRresidPlck      -0.251    0.104   -2.413    0.016   -0.251   -0.900

# Variances:
#                Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logPlckTons       0.108    0.042    2.550    0.011    0.108    0.091
# logPlckTAC        0.600    0.235    2.550    0.011    0.600    0.650
# logPlckAdults     0.351    0.137    2.550    0.011    0.351    0.380
# SRresidPlck       0.721    0.283    2.550    0.011    0.721    0.467
# logCopepods       0.487    0.191    2.550    0.011    0.487    0.528
# logAnnChl         0.520    0.204    2.550    0.011    0.520    0.563
# WTemp             0.703    0.276    2.550    0.011    0.703    0.762

# R-Square:
#                Estimate
# logPlckTons       0.909
# logPlckTAC        0.350
# logPlckAdults     0.620
# SRresidPlck       0.533
# logCopepods       0.472
# logAnnChl         0.437
# WTemp             0.238



# add Halibut predation to Adult Pollock regression:
mod.Pol8 <- 'logPlckTons ~ logPlckTAC
logPlckTAC ~ logPlckAdults.lead1
logPlckAdults ~ logPlckRecruitSums + logPlckAdults.lead1 + logHlbt
SRresidPlck ~ logCopepods + logAnnChl + WTemp + logPlckAdults + logArrAdult
logCopepods ~ PDO
logAnnChl ~ NPGO
WTemp ~ NPGO

'
mod.Pol8.fit <- sem(mod.Pol8, data=CPrPlckLags3) 
summary(mod.Pol8.fit, stand=T, rsq=T)

# logPlckAdults ~ logHlbt is almost significant (p = 0.051) and the coefficient is strong and has the correct sign (negative), 
# but why is the overall model fit so much worse???
# lavaan (0.5-20) converged normally after  36 iterations

# Number of observations                            13
# Estimator                                         ML
# Minimum Function Test Statistic              550.797
# Degrees of freedom                                49
# P-value (Chi-square)                           0.000

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logPlckTons ~                                                         
#   logPlckTAC        1.078    0.046   23.384    0.000    1.078    0.953
# logPlckTAC ~                                                          
#   lgPlckAdlts.l1   -0.592    0.224   -2.645    0.008   -0.592   -0.592
# logPlckAdults ~                                                       
#   logPlckRcrtSms    0.699    0.240    2.912    0.004    0.699    0.699
#   lgPlckAdlts.l1    0.477    0.217    2.196    0.028    0.477    0.477
#   logHlbt          -0.590    0.302   -1.950    0.051   -0.590   -0.590
# SRresidPlck ~                                                         
#   logCopepods      -0.203    0.112   -1.818    0.069   -0.203   -0.157
#   logAnnChl         0.654    0.122    5.338    0.000    0.654    0.503
#   WTemp             0.322    0.116    2.788    0.005    0.322    0.248
#   logPlckAdults     0.184    0.123    1.497    0.134    0.184    0.142
#   logArrAdult       0.665    0.121    5.496    0.000    0.665    0.512
# logCopepods ~                                                         
#   PDO               0.687    0.202    3.410    0.001    0.687    0.687
# logAnnChl ~                                                           
#   NPGO              0.661    0.208    3.174    0.002    0.661    0.661
# WTemp ~                                                               
#   NPGO             -0.488    0.242   -2.017    0.044   -0.488   -0.488

# Covariances:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logPlckTons ~~                                                        
#   SRresidPlck      -0.251    0.104   -2.413    0.016   -0.251   -0.900

# Variances:
#                Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logPlckTons       0.108    0.042    2.550    0.011    0.108    0.091
# logPlckTAC        0.600    0.235    2.550    0.011    0.600    0.650
# logPlckAdults     0.271    0.106    2.550    0.011    0.271    0.294
# SRresidPlck       0.721    0.283    2.550    0.011    0.721    0.463
# logCopepods       0.487    0.191    2.550    0.011    0.487    0.528
# logAnnChl         0.520    0.204    2.550    0.011    0.520    0.563
# WTemp             0.703    0.276    2.550    0.011    0.703    0.762

# R-Square:
#                Estimate
# logPlckTons       0.909
# logPlckTAC        0.350
# logPlckAdults     0.706
# SRresidPlck       0.537
# logCopepods       0.472
# logAnnChl         0.437
# WTemp             0.238
