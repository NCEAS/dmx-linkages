################################################################
## Communities of Practice Structural Equation Modelling Script
## Colette Ward 23 Jan 2016
################################################################

library(plyr)
library(dplyr)
library(lavaan)
library(AICcmodavg)

# call the data assembly script
#source("commPracticeDataFormat.R")
#CPrD <- CoPrct

# or load the data from local source
CPrD <- read.csv("CoPrct.csv", header=T)
head(CPrD)

# select 1998 - 2010
CPrD1 <- CPrD %>%
  filter(Year > 1997 & Year < 2011)

# log-transform and rename some variables:
CPrD2 <- CPrD1 %>%
  mutate(logEuphausiids = log(SewardLineMayEuphausiids)) %>%
  mutate(logCopepods = log(SewardLineMayCopepods)) %>%
  mutate(logPinkShrimp = log(Pink_Shrimp)) %>%
  mutate(logCapelin = log(Capelin)) %>%
  mutate(logStellerAdult = log(SSLnonPup_anul_mn)) %>%
  mutate(logPlckRecruits = log(Poll_Age1_recruits_millions)) %>%
  mutate(logPlckAdults = log(Poll_Yr3plus_TtlBmss_1000Tons)) %>%
  mutate(logArrAdult = log(ArrAdult)) %>%
  mutate(logPCodFem = log(PCod_female_Bmss_t)) %>%
  mutate(logPCodRecruits = log(PCod_Age1_millions)) %>%
  mutate(logPlckTons = log(plck_tons)) %>%
  mutate(logPlckVessels = log(plck_vessels)) %>%
  mutate(logHlbtPounds = log(hlbt_pounds)) %>%
  mutate(logArrTons = log(arth_tons)) %>%
  mutate(logArrRev = log(arth_real_rev)) %>%
  mutate(logArrVessels = log(arth_vessels)) %>%
  mutate(logArrProcess = log(arth_processors)) %>%
  mutate(logArrPrice = log(arth_real_price)) %>%
  rename(SLPress = SeaLevelPressure_mean_hPa, WindDirAn = WndDir_degT_AnnMn, WindSpAn = WndSp_m_s_AnnMn, 
         WindDirWin = WndDir_degT_Winter, WindSpWin = WndSp_m_s_Winter, ENSO = ENSO_anul_mn, NPGO = NPGO_anul_mn,
         PDO = PDO_anul_mn, UpwellAnom = UpWelAnom_anul_mn, Ekman = EKE_ann_max_mean, WTemp = WTemp_C_AnnMn, 
         logAnnChl = AnnChl, PinkSal_SSB = PWS_WildPinkSalmon_SSB_ModelOutput, SharkAbund = SharkAbundIPHC, 
         PlckPrice = plck_real_price_SAFE) %>%
  select(-SewardLineMayEuphausiids, -SewardLineMayCopepods, -Pink_Shrimp, -Poll_Age1_recruits_millions,
         -Poll_Yr3plus_TtlBmss_1000Tons, -ArrAdult, -PCod_female_Bmss_t, -PCod_Age1_millions, -plck_tons, -plck_vessels,
         -hlbt_pounds, -arth_tons, -arth_real_rev, -arth_vessels, -arth_processors, -arth_real_price, -Capelin,
         -SSLnonPup_anul_mn)
names(CPrD2)

# standardize each variable to zero mean and unit variance
CPrD3 <- CPrD2 %>% colwise(scale)()



#############################################
# 1. Arrowtooth model 
# see slide at: https://docs.google.com/a/noaa.gov/presentation/d/1GPMfcLRIXkg1ZEndSmMcdMFaa3IkU6Lbbs7QabwFewU/edit?usp=sharing
#############################################

# inspect raw variables
attach(CPrD1)
par(mfrow=c(3,5));
hist(ENSO); hist(NPGO); hist(PDO); 
hist(WndSp_m_s_Winter); hist(WTemp_C_AnnMn); hist(AnnChl); 
hist(log(SewardLineMayEuphausiids)); hist(log(Capelin)); hist(log(Poll_Age1_recruits_millions)); 
hist(log(ArrAdult)); hist(log(arth_tons)); hist(log(arth_real_rev)); 
hist(log(arth_vessels)); hist(arth_processors); hist(arth_real_price)
detach(CPrD1)



# Candidate models
# full model, if we had all the data:
mod.1 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logCapelin + ArrowtoothJuv
logPlckRecruits ~ logEuphausiids + ArrowtoothJuv
logCapelin ~ logEuphausiids
ArrowtoothJuv ~ logCapelin + logEuphausiids
logEuphausiids ~ logAnnChl
ArrowtoothJuv ~ LarvalAbundance
logAnnChl ~ WTemp
WTemp ~ ENSO + PDO
LarvalAbundance ~ ENSO + PDO + NPGO'



############################################################


# full model after removing nodes for which we have little/no data (Wind, Capelin, ATF larvae & juveniles)
# also, cannot have both ENSO & PDO in the model because they're highly correlated; keep ENSO because it's less correlated with the NPGO
mod.2 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + ENSO + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ ENSO'
mod.2.fit <- sem(mod.2, data=CPrD3)
summary(mod.2.fit, stand=T, rsq=T)
#lavaan (0.5-20) converged normally after  19 iterations
#Number of observations                            13
#Estimator                                         ML
#Minimum Function Test Statistic               50.442
#Degrees of freedom                                18
#P-value (Chi-square)                           0.000

modindices(mod.2.fit)
# Residual covarations to consider with modificaiton index:
# logArrTons ~~ logPlckRecruits 7.898
# logArrAdult ~~ logAnnChl 5.786
# logArrAdult ~~ logEuphausiids 4.002

# Direct links to consider, with modification index:
#logArrTons  ~ logPlckRecruits 7.666
#logArrTons  ~  ENSO 4.254
#logArrAdult  ~ logAnnChl 4.002
#logPlckRecruits  ~ ENSO 3.409
#logAnnChl  ~ NPGO 4.140
#WTemp  ~  NPGO 3.105


############################################################

# Add residual covariation between Arrowtooth harvest and Pollock recruitment:
mod.3 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + ENSO + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ ENSO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.3.fit <- sem(mod.3, data=CPrD3)
summary(mod.3.fit, stand=T, rsq=T)
#Minimum Function Test Statistic               38.156
#Degrees of freedom                                17
#P-value (Chi-square)                           0.002


modindices(mod.3.fit)
# Residual covarations to consider, with modificaiton index:
#logArrAdult ~~ logEuphausiids 4.002
#logArrAdult ~~ logAnnChl 5.786


# Direct links to consider, with modification index:
# logArrAdult  ~ logAnnChl 4.002 (but it's hard to imagine how there is a DIRECT effect of Chl a on Adult Arrowtooth biomass)
# logArrAdult  ~ WTemp 2.211
# logAnnChl  ~  NPGO 4.140 (suggests effect is direct instead of via Water Temperature)
# logAnnChl  ~  ENSO 1.860 (suggests effect is direct instead of via Water Temperature)
# WTemp  ~ logAnnChl 1.860
# WTemp  ~ NPGO 3.105


# greatest modification index for direct links to consider was logAnnChl  ~ NPGO. Also note mod index for WTemp  ~  NPGO
# therefore for next model: add NPGO as driver of Water Temperature and look at mod index for logAnnChl  ~ NPGO


############################################################


mod.4 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + ENSO + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ WTemp
WTemp ~ ENSO + NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.4.fit <- sem(mod.4, data=CPrD3)
summary(mod.4.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               34.608
# Degrees of freedom                                16
# P-value (Chi-square)                           0.003

modindices(mod.4.fit)
# Residual covarations to consider, with modificaiton index:
# logArrAdult ~~  logEuphausiids 4.053
# logArrAdult ~~ logAnnChl 5.786
# logArrAdult ~~  WTemp 2.904
# logAnnChl ~~ WTemp 4.702

# Direct links to consider, with modification index:
# logArrAdult  ~  logAnnChl 4.052
# logArrAdult  ~  WTemp 2.904
# logAnnChl  ~  NPGO 5.414

# ENSO effect on WaterTemp is not significant (p = 0.622), therefore remove it
# NPGO has a significant effect on WaterTemp (p = 0.043)
# modification index suggests NPGO also has direct effect on Chl a
# therefore add this in next model

############################################################

mod.5 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + ENSO + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ WTemp + NPGO
WTemp ~ NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.5.fit <- sem(mod.5, data=CPrD3)
summary(mod.5.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               27.847
# Degrees of freedom                                16
# P-value (Chi-square)                           0.033
# Note big improvement in model fit, though still not statistically significant

modindices(mod.5.fit)
# Residual covarations to consider, with modificaiton index:
# logArrAdult ~~  logEuphausiids 7.084
# logArrAdult ~~       logAnnChl 5.999
# logArrAdult ~~           WTemp 2.851 (does any)

# Direct links to consider, with modification index:
# logArrAdult  ~       logAnnChl 7.084
# logArrAdult  ~           WTemp 2.851


# Regressions:
#                   Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                           
#   logArrAdult        0.848    0.085    9.952    0.000    0.848    0.851
# logArrAdult ~                                                          
#   logPlckRecruts     0.087    0.227    0.384    0.701    0.087    0.086
#   logEuphausiids     0.402    0.230    1.749    0.080    0.402    0.390
#   ENSO              -0.183    0.259   -0.705    0.481   -0.183   -0.177
#   NPGO              -0.538    0.259   -2.077    0.038   -0.538   -0.523
# logPlckRecruits ~                                                      
#   logEuphausiids     0.168    0.180    0.932    0.351    0.168    0.166
# logEuphausiids ~                                                       
#   logAnnChl          0.062    0.277    0.223    0.824    0.062    0.062
# logAnnChl ~                                                            
#   WTemp              0.120    0.236    0.509    0.611    0.120    0.120
#   NPGO               0.719    0.236    3.046    0.002    0.719    0.719
# WTemp ~                                                                
#   NPGO              -0.488    0.242   -2.018    0.044   -0.488   -0.488


# From the above, looks like WaterTemperature is not important as a link between NPGO and Chl a (p = 0.61)
# therefore in next model, remove this link

############################################################


mod.6 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + ENSO + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.6.fit <- sem(mod.6, data=CPrD3)
summary(mod.6.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               20.390
# Degrees of freedom                                11
# P-value (Chi-square)                           0.040


modindices(mod.6.fit)
# Residual covarations to consider, with modificaiton index:
# logArrAdult ~~  logEuphausiids 7.084
# logArrAdult ~~       logAnnChl 7.084

# Direct links to consider, with modification index:
# logArrAdult  ~       logAnnChl 7.084 (how do I know this is not associated with effect of NPGO?)


# Regressions:
#                   Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                           
#   logArrAdult        0.848    0.085    9.952    0.000    0.848    0.851
# logArrAdult ~                                                          
#   logPlckRecruts     0.087    0.227    0.384    0.701    0.087    0.086
#   logEuphausiids     0.402    0.230    1.749    0.080    0.402    0.390
#   ENSO              -0.183    0.259   -0.705    0.481   -0.183   -0.177
#   NPGO              -0.538    0.259   -2.077    0.038   -0.538   -0.523
# logPlckRecruits ~                                                      
#   logEuphausiids     0.168    0.180    0.932    0.351    0.168    0.166
# logEuphausiids ~                                                       
#   logAnnChl          0.062    0.277    0.223    0.824    0.062    0.062
# logAnnChl ~                                                            
#   NPGO               0.661    0.208    3.174    0.002    0.661    0.661

# Covariances:
#                   Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~~                                                         
#   logPlckRecruts    0.347    0.156    2.222    0.026    0.347    0.784


# and no significant direct effect of ENSO on Arrowtooth Adult biomass
# therefore remove it from next model


############################################################

mod.7 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.7.fit <- sem(mod.7, data=CPrD3)
summary(mod.7.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               12.614
# Degrees of freedom                                 7
# P-value (Chi-square)                           0.082

modindices(mod.7.fit)
# Residual covarations to consider, with modificaiton index:
# logArrAdult ~~  logEuphausiids 6.908
# logArrAdult ~~       logAnnChl 6.909

# Direct links to consider, with modification index:
# logArrAdult  ~       logAnnChl 6.909


# Regressions:
#                   Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                           
#   logArrAdult        0.848    0.085    9.956    0.000    0.848    0.833
# logArrAdult ~                                                          
#   logPlckRecruts     0.169    0.230    0.732    0.464    0.169    0.164
#   logEuphausiids     0.380    0.233    1.631    0.103    0.380    0.366
#   NPGO              -0.460    0.230   -2.005    0.045   -0.460   -0.444
# logPlckRecruits ~                                                      
#   logEuphausiids     0.168    0.180    0.934    0.350    0.168    0.166
# logEuphausiids ~                                                       
#   logAnnChl          0.062    0.277    0.223    0.824    0.062    0.062
# logAnnChl ~                                                            
#   NPGO               0.661    0.208    3.174    0.002    0.661    0.661

# from the above, looks like Pollock Recruits direct pathway is not important 
# (but residual covariation with Arrowtooth Harvest suggests it's important via another route)
# next model: what happens if I remove the residual covariation between pollock recruits & Arrowtooth Harvest?

############################################################


mod.8 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logPlckRecruits + logEuphausiids + NPGO
logPlckRecruits ~ logEuphausiids
logEuphausiids ~ logAnnChl
logAnnChl ~ NPGO

'

mod.8.fit <- sem(mod.8, data=CPrD3)
summary(mod.8.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               24.900
# Degrees of freedom                                 8
# P-value (Chi-square)                           0.002

# Note model fit is worse. Therefore leave in logArrTons ~~ logPlckRecruits
# next step: remove Pollock Recruits pathway

############################################################

# note we are only left with the Euphausiid pathway

mod.9 <- 'logArrTons ~ logArrAdult
logArrAdult ~ logEuphausiids + NPGO
logEuphausiids ~ logAnnChl
logAnnChl ~ NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.9.fit <- sem(mod.9, data=CPrD3)
summary(mod.9.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               13.990
# Degrees of freedom                                 9
# P-value (Chi-square)                           0.123

# Compare to mod.7: 
# Test statistic 12.614, df = 7, p-value = 0.082


# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                          
#   logArrAdult       0.828    0.084    9.866    0.000    0.828    0.867
# logArrAdult ~                                                         
#   logEuphausiids    0.394    0.234    1.684    0.092    0.394    0.387
#   NPGO             -0.432    0.234   -1.847    0.065   -0.432   -0.424
# logEuphausiids ~                                                      
#   logAnnChl         0.062    0.277    0.223    0.824    0.062    0.062
# logAnnChl ~                                                           
#   NPGO              0.661    0.208    3.174    0.002    0.661    0.661

# Covariances:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~~                                                         
#   logPlckRecruts    0.344    0.157    2.201    0.028    0.344    0.771

# Direct effect of Chl a on Euphausiids is not significant ... so the food web path cannot be mediating here ...?
# (but then when I remove the food web pathway below in mod.10, NPGO direct effect on ArrowtoothAdults is not significant ...)

modindices(mod.9.fit)
# Residual covariations to consider, with modification index:
# logArrAdult ~~       logAnnChl 5.837

# # Direct links to consider, with modification index:
# logArrAdult  ~       logAnnChl 5.837

############################################################


# minimum model (without Euphausiid pathway):
mod.10 <- 'logArrTons ~ logArrAdult
logArrAdult ~ NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.10.fit <- sem(mod.10, data=CPrD3)
summary(mod.10.fit, stand=T, rsq=T)
# Minimum Function Test Statistic                1.840
# Degrees of freedom                                 3
# P-value (Chi-square)                           0.606

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                          
#   logArrAdult       0.828    0.086    9.675    0.000    0.828    0.863
# logArrAdult ~                                                         
#   NPGO             -0.370    0.258   -1.437    0.151   -0.370   -0.370

# Covariances:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~~                                                         
#   logPlckRecruts    0.344    0.157    2.201    0.028    0.344    0.771

# Note that logArrAdult ~ NPGO direct effect is not significant ...

############################################################

# try adding Adult Pollock Biomass to look at pollock fishery hypothesis, 
# and remove residual covariation between Arrowtooth harvest and Pollock recruits
mod.11 <- 'logArrTons ~ logArrAdult + logPlckAdults
logArrAdult ~ logEuphausiids + NPGO
logEuphausiids ~ logAnnChl
logAnnChl ~ NPGO

'

mod.11.fit <- sem(mod.11, data=CPrD3)
summary(mod.11.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               12.817
# Degrees of freedom                                 8
# P-value (Chi-square)                           0.118

# model fit is similar to mod.9, worse than mod.10
# direct effect of Adult Pollock on Arrowtooth harvest is not significant (p = 0.22)
# NB model fit is worse when residual covariation between Pollock recruits and Arrowtooth harvest is added back in

modindices(mod.11.fit)

############################################################


# try adding Pollock harvest (tons)
# Adult Pollock Biomass and Pollock Harvest (tons) are not correlated so it's OK to add both

mod.12 <- 'logArrTons ~ logArrAdult + logPlckAdults + logPlckTons
logArrAdult ~ logEuphausiids + NPGO
logEuphausiids ~ logAnnChl
logAnnChl ~ NPGO

'

mod.12.fit <- sem(mod.12, data=CPrD3)
summary(mod.12.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               32.744
# Degrees of freedom                                11
# P-value (Chi-square)                           0.001                         

# Note model fit is much worse than most previous models
# fit is just as poor when first line is logArrTons ~ logArrAdult + logPlckTons
# However, direct effect of Pollock harvest (tons) on Arrowtooth harvest is significant (p = 0.042), although weak (0.19)


# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                          
#   logArrAdult       1.071    0.115    9.314    0.000    1.071    0.885
#   logPlckAdults    -0.169    0.118   -1.436    0.151   -0.169   -0.137
#   logPlckTons       0.239    0.117    2.035    0.042    0.239    0.194
# logArrAdult ~                                                         
#   logEuphausiids    0.394    0.234    1.684    0.092    0.394    0.387
#   NPGO             -0.432    0.234   -1.847    0.065   -0.432   -0.424
# logEuphausiids ~                                                      
#   logAnnChl         0.062    0.277    0.223    0.824    0.062    0.062
# logAnnChl ~                                                           
#   NPGO              0.661    0.208    3.174    0.002    0.661    0.661



modindices(mod.12.fit)
# direct links to consider, with modification index:
# logArrAdult  ~    logPlckTons 4.602
# logPlckTons  ~ logEuphausiids 4.011 (yet for mod.11 above, with Adult Pollock instead of Pollock Harvest, modification index for logPlckAdults  ~  logEuphausiids 0.126)


############################################################


# remove Adult Pollock Biomass
# and try removing NPGO -> Chl a -> Euphausiid pathway
mod.13 <- 'logArrTons ~ logArrAdult + logPlckTons
logArrAdult ~ logEuphausiids + NPGO

'

mod.13.fit <- sem(mod.13, data=CPrD3)
summary(mod.13.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               10.668
# Degrees of freedom                                 3
# P-value (Chi-square)                           0.014

# Still not a great fit - even though nodes are all marginally significant?

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                          
#   logArrAdult       1.026    0.127    8.062    0.000    1.026    0.925
#   logPlckTons       0.224    0.127    1.759    0.078    0.224    0.202
# logArrAdult ~                                                         
#   logEuphausiids    0.394    0.237    1.664    0.096    0.394    0.394
#   NPGO             -0.432    0.237   -1.826    0.068   -0.432   -0.432



modindices(mod.13.fit)
# direct links to consider:
# logPlckTons  ~    logArrAdult 4.788 (consider Arrowtooth predation on pollock?)
# logPlckTons  ~     logArrTons 4.080
# logArrAdult  ~    logPlckTons 6.731


############################################################

# Add back residual covariation between Arrowtooth harvest and Pollock Recruits

mod.14 <- 'logArrTons ~ logArrAdult + logPlckTons
logArrAdult ~ logEuphausiids + NPGO

#Residual covaration
logArrTons ~~ logPlckRecruits

'

mod.14.fit <- sem(mod.14, data=CPrD3)
summary(mod.14.fit, stand=T, rsq=T)
# Minimum Function Test Statistic               10.987
# Degrees of freedom                                 7
# P-value (Chi-square)                           0.139

# much better fit now

# Regressions:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~                                                          
#   logArrAdult       0.997    0.069   14.549    0.000    0.997    0.918
#   logPlckTons       0.255    0.069    3.724    0.000    0.255    0.235
# logArrAdult ~                                                         
#   logEuphausiids    0.394    0.237    1.664    0.096    0.394    0.394
#   NPGO             -0.432    0.237   -1.826    0.068   -0.432   -0.432

# Covariances:
#                  Estimate  Std.Err  Z-value  P(>|z|)   Std.lv  Std.all
# logArrTons ~~                                                         
#   logPlckRecruts    0.355    0.153    2.327    0.020    0.355    0.845

modindices(mod.14.fit)
# direct links to consider, with modification indices:
# logEuphausiids  ~     logArrAdult 2.454 (predation?)

############################################################


# Compare model fits:
anova(mod.x.fit, mod.y.fit)

# Chi Square Difference Test *** ONLY for nested models! ***




aictab(list(mod.7.fit, mod.9.fit, mod.10.fit, mod.11.fit, mod.12.fit, mod.13.fit, mod.14.fit), second.ord=T, sort=T)


# now calculate and plot residuals ...


##########################################################################################
##########################################################################################
##########################################################################################

# 2. Pollock model 
# see slide x at: https://docs.google.com/a/noaa.gov/presentation/d/1GPMfcLRIXkg1ZEndSmMcdMFaa3IkU6Lbbs7QabwFewU/edit?usp=sharing

# inspect variables
attach(CPrD1)
par(mfrow=c(3,5));
hist(ENSO_anul_mn); hist(NPGO_anul_mn); hist(PDO_anul_mn); 
hist(WTemp_C_AnnMn); hist(AnnChl); hist(log(Euphausiids)); 
hist(log(MayCopepods)); hist(log(Pink_Shrimp)); hist(log(Poll_Age1_recruits_millions)); 
hist(log(Poll_Yr3plus_TtlBmss_1000Tons)); hist(log(ArrAdult)); hist(log(PCod_female_Bmss_t)); 
hist(log(hlbt_pounds)); hist(log(plck_tons))
detach(CPrD1)


# model after removing nodes for which we have little/no data (Wind, Transport, juvenile growth & abundance)
mod.6 <- 'plck_tons ~ Poll_Yr3plus_TtlBmss_1000Tons
Poll_Yr3plus_TtlBmss_1000Tons ~ Poll_Age1_recruits_millions + Euphausiids + Pink_Shrimp + ArrAdult + PCod_female_Bmss_t
Poll_Age1_recruits_millions ~ Euphausiids + Copepods + AnnChl + WTemp_C_AnnMn + Poll_Yr3plus_TtlBmss_1000Tons
Euphausiids ~ AnnChl
Copepods ~ AnnChl
AnnChl ~ WTemp_C_AnnMn
WTemp_C_AnnMn ~ NPGO_anul_mn + ENSO_anul_mn + PDO_anul_mn'
mod.6.fit <- sem(mod.6, data=CPrD3) # model does not converge. and nobs (13) < nvar (14)
summary(mod.6.fit, stand=T, rsq=T)


