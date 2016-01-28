################################################################
## Communities of Practice Structural Equation Modelling Script
## created 23 Jan 2016
################################################################

library(dplyr)
library(lavaan)
library(AICcmodavg)

# call the data assembly script
source("commPracticeDataFormat.R")
CPrD <- CoPrct

# or load the data from local source
CPrD <- read.csv("CoPrct.csv", header=T)


# select 1998 - 2010
CPrD1 <- CPrD %>%
  filter(Year > 1997) %>%
  filter(Year < 2011)

# log-transform some variables:
CPrD2 <- CPrD1 %>%
  mutate(Euphausiids = log(Euphausiids)) %>%
  mutate(MayCopepods = log(MayCopepods)) %>%
  mutate(Pink_Shrimp = log(Pink_Shrimp)) %>%
  mutate(Poll_Age1_recruits_millions = log(Poll_Age1_recruits_millions)) %>%
  mutate(Poll_Yr3plus_TtlBmss_1000Tons = log(Poll_Yr3plus_TtlBmss_1000Tons)) %>%
  mutate(ArrAdult = log(ArrAdult)) %>%
  mutate(PCod_female_Bmss_t = log(PCod_female_Bmss_t)) %>%
  mutate(plck_tons = log(plck_tons)) %>%
  mutate(hlbt_pounds = log(hlbt_pounds)) %>%
  mutate(arth_tons = log(arth_tons)) %>%
  mutate(arth_real_rev = log(arth_real_rev)) %>%
  mutate(arth_vessels = log(arth_vessels)) %>%
  mutate(arth_processors = log(arth_processors)) %>%
  mutate(arth_real_price = log(arth_real_price))


# standardize each variable to zero mean and unit variance
CPrD3 <- CPrD2 %>% 
  colwise(scale)()


#############################################
# 1. Arrowtooth model 
# see slide 8 at: https://docs.google.com/a/noaa.gov/presentation/d/1GPMfcLRIXkg1ZEndSmMcdMFaa3IkU6Lbbs7QabwFewU/edit?usp=sharing
#############################################

# inspect variables
attach(CPrD1)
par(mfrow=c(3,5));
hist(ENSO_anul_mn); hist(NPGO_anul_mn); hist(PDO_anul_mn); 
hist(WndSp_m_s_Winter); hist(WTemp_C_AnnMn); hist(AnnChl); 
hist(log(Euphausiids)); hist(log(Capelin)); hist(log(Poll_Age1_recruits_millions)); 
hist(log(ArrAdult)); hist(log(arth_tons)); hist(log(arth_real_rev)); 
hist(log(arth_vessels)); hist(arth_processors); hist(arth_real_price)
detach(CPrD1)



# Candidate models
# full model, if we had all the data:
mod.4 <- 'arth_tons ~ ArrAdult
ArrAdult ~ Poll_Age1_recruits_millions + Capelin + ArrowtoothJuv
Poll_Age1_recruits_millions ~ Euphausiids + ArrowtoothJuv
Capelin ~ Euphausiids
ArrowtoothJuv ~ Capelin + Euphausiids
Euphausiids ~ AnnChl
ArrowtoothJuv ~ LarvalAbundance
AnnChl ~ WTemp_C_AnnMn
WTemp_C_AnnMn ~ ENSO_anul_mn + PDO_anul_mn
LarvalAbundance ~ ENSO_anul_mn + PDO_anul_mn + NPGO_anul_mn'




# full model after removing nodes for which we have little/no data (Wind, Capelin, ATF larvae & juveniles)
mod.5 <- 'arth_tons ~ ArrAdult
ArrAdult ~ Poll_Age1_recruits_millions + Euphausiids + ENSO_anul_mn + PDO_anul_mn + NPGO_anul_mn
Poll_Age1_recruits_millions ~ Euphausiids
Euphausiids ~ AnnChl
AnnChl ~ WTemp_C_AnnMn
WTemp_C_AnnMn ~ ENSO_anul_mn + PDO_anul_mn'
mod.5.fit <- sem(mod.5, data=CPrD3)
summary(mod.5.fit, stand=T, rsq=T)



# minimum model:
mod.min <- 'arth_tons ~ ENSO_anul_mn + PDO_anul_mn + NPGO_anul_mn'
mod.min.fit <- sem(mod.min, data=CPrD3)
summary(mod.min.fit, stand=T, rsq=T)



# Using ANOVA function to compare models:
anova(mod.5.fit, mod.min.fit)



# now calculate and plot residuals 


#############################################
# 2. Pollock model 
# see slide 9 at: https://docs.google.com/a/noaa.gov/presentation/d/1GPMfcLRIXkg1ZEndSmMcdMFaa3IkU6Lbbs7QabwFewU/edit?usp=sharing
#############################################

# inspect variables
attach(CPrD1)
par(mfrow=c(3,5));
hist(ENSO_anul_mn); hist(NPGO_anul_mn); hist(PDO_anul_mn); 
hist(WTemp_C_AnnMn); hist(AnnChl); hist(log(Euphausiids)); 
hist(log(MayCopepods)); hist(log(Pink_Shrimp)); hist(log(Poll_Age1_recruits_millions)); 
hist(log(Poll_Yr3plus_TtlBmss_1000Tons)); hist(log(ArrAdult)); hist(log(PCod_female_Bmss_t)); 
hist(log(hlbt_pounds)); hist(log(plck_tons))
detach(CPrD1)


# full model after removing nodes for which we have little/no data (Wind, Transport, juvenile growth & abundance)
mod.6 <- 'plck_tons ~ Poll_Yr3plus_TtlBmss_1000Tons
Poll_Yr3plus_TtlBmss_1000Tons ~ Poll_Age1_recruits_millions + Euphausiids + Pink_Shrimp + ArrAdult + hlbt_pounds + PCod_female_Bmss_t
Poll_Age1_recruits_millions ~ Euphausiids + MayCopepods + AnnChl + NPGO_anul_mn + WTemp_C_AnnMn + Poll_Yr3plus_TtlBmss_1000Tons
Euphausiids ~ AnnChl + WTemp_C_AnnMn
MayCopepods ~ AnnChl + NPGO_anul_mn
AnnChl ~ WTemp_C_AnnMn + NPGO_anul_mn + ENSO_anul_mn + PDO_anul_mn
WTemp_C_AnnMn ~ NPGO_anul_mn + ENSO_anul_mn + PDO_anul_mn'
mod.6.fit <- sem(mod.6, data=CPrD3)
summary(mod.6.fit, stand=T, rsq=T)


