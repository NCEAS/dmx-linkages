###############################################
### DMX Linkages Group 
### Random Forest Analysis Script
### Started by Rachael Blake    January 2016
###############################################

library(randomForest);library(plyr);library(dplyr)

# call the data assembly script

source("commPracticeDataFormat.R")


#############
# FUNCTION to run randomForest analysis and print outputs and plots
#############
# DOESN'T WORK YET!!!
RForestRun <- function(subset_df, resp_column){
              a <- randomForest(resp_column ~., data=subset_df, importance=T, 
                                do.trace=1000, ntree=5000)
              b <- print(a)   
              c <- plot(a)
              d <- varImpPlot(a)
              e <- a$importance
              rflist <- c(a,b,c,d,e)
              return(rflist)
              }


####
# Important variables are greater than the absolute value of the most negative
# importance value.
Imp_Var <- function(rf_obj){
           a <- data.frame(rf_obj$importance)  
           b <- min(a[,1])  # smallest value
           c <- abs(b)  # absolute value of smallest value
           d <- a$X.IncMSE[a$X.IncMSE>c]  # numbers great than absolute value of smallest value
           return(d)
           }

#############
#############

# transform some variables 
CPD <- CoPrct %>%
       mutate(log_WTemp_C_AnnMn = log(WTemp_C_AnnMn),
              log_SwdLineMayEuphausiids = log(SewardLineMayEuphausiids),
              log_SwdLineMayCopepods = log(SewardLineMayCopepods),
              log_Poll_Age1_recruits_millions = log(Poll_Age1_recruits_millions),
              log_Pink_Shrimp = log(Pink_Shrimp),
              log_hlbt_pounds = log(hlbt_pounds)
              )
       
############

###
# Pink Salmon
CPD_Pink <- CPD %>% 
            select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                   log_WTemp_C_AnnMn,AnnChl,log_SwdLineMayEuphausiids,
                   log_SwdLineMayCopepods,goaPinkCatchNum,
                   PWS_WildPinkSalmon_SSB_ModelOutput,ArrAdult,
                   Poll_Yr3plus_TtlBmss_1000Tons) %>%
            filter(complete.cases(.))
goaPink <- randomForest(PWS_WildPinkSalmon_SSB_ModelOutput ~., data=CPD_Pink, importance=T, do.trace=1000, ntree=5000)
print(goaPink)
plot(goaPink)
varImpPlot(goaPink)
goaPink$importance
Imp_Var(goaPink) # get important variables
#  important variables below
partialPlot(goaPink,CPD_Pink,goaPinkCatchNum)



# Arrowtooth   ## NOTE: Capelin left out due to too few data points
CPD_Arrow <- CPD %>% 
             select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                    log_WTemp_C_AnnMn,AnnChl,log_SwdLineMayEuphausiids,
                    log_Poll_Age1_recruits_millions, ArrAdult,arth_tons,
                    arth_real_rev,arth_vessels,arth_processors,arth_real_price) %>%
             filter(complete.cases(.))
arrow <- randomForest(ArrAdult ~., data=CPD_Arrow, importance=T, do.trace=1000, ntree=5000)
print(arrow)
plot(arrow)
varImpPlot(arrow)
arrow$importance
Imp_Var(arrow) # get important variables
# important variables below
partialPlot(arrow,CPD_Arrow,NPGO_anul_mn)
partialPlot(arrow,CPD_Arrow,AnnChl)
partialPlot(arrow,CPD_Arrow,arth_tons)
partialPlot(arrow,CPD_Arrow,arth_real_rev)
partialPlot(arrow,CPD_Arrow,arth_vessels)




# Pollock Adults
CPD_Poll <- CPD %>% 
            select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                   log_WTemp_C_AnnMn,AnnChl,EKE_ann_max_mean,log_SwdLineMayEuphausiids,
                   log_SwdLineMayCopepods,log_Pink_Shrimp,
                   log_Poll_Age1_recruits_millions,Poll_Yr3plus_TtlBmss_1000Tons,
                   ArrAdult, PCod_female_Bmss_t,hlbt_pounds,
                   plck_tons,plck_real_rev,plck_vessels,plck_processors) %>%
            filter(complete.cases(.))
poll_a <- randomForest(Poll_Yr3plus_TtlBmss_1000Tons ~., data=CPD_Poll, importance=T, do.trace=1000, ntree=5000)
print(poll_a)
plot(poll_a)
varImpPlot(poll_a)
poll_a$importance
Imp_Var(poll_a) # get important variables
# important variables below
partialPlot(poll_a,CPD_Poll,log_WTemp_C_AnnMn)
partialPlot(poll_a,CPD_Poll,log_SwdLineMayCopepods)
partialPlot(poll_a,CPD_Poll,log_Poll_Age1_recruits_millions)
partialPlot(poll_a,CPD_Poll,ArrAdult)



# Pollock Juvenile
CPD_JPoll <- CPD %>%
             select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                    log_WTemp_C_AnnMn,AnnChl, EKE_ann_max_mean,log_SwdLineMayEuphausiids,
                    log_SwdLineMayCopepods,log_Poll_Age1_recruits_millions,
                    Poll_Yr3plus_TtlBmss_1000Tons,ArrAdult) %>%
             filter(complete.cases(.))
poll_j <- randomForest(log_Poll_Age1_recruits_millions ~., data=CPD_JPoll, importance=T, do.trace=1000, ntree=5000)
print(poll_j)
plot(poll_j)
varImpPlot(poll_j)
poll_j$importance
Imp_Var(poll_j) # get important variables
# important variables below
partialPlot(poll_j,CPD_JPoll,PDO_anul_mn)
partialPlot(poll_j,CPD_JPoll,Poll_Yr3plus_TtlBmss_1000Tons)



# Halibut
CPD_Hal <- CPD %>% 
           select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                  log_WTemp_C_AnnMn,AnnChl,Poll_Yr3plus_TtlBmss_1000Tons, 
                  log_Poll_Age1_recruits_millions, TotTCrab,log_Pink_Shrimp,
                  log_hlbt_pounds,hlbt_real_rev,hlbt_vessels,hlbt_processors) %>%
           filter(complete.cases(.))
       
hlbt_lbs <- randomForest(log_hlbt_pounds ~., data=CPD_Hal, importance=T, do.trace=1000, ntree=5000)
print(hlbt_lbs)
plot(hlbt_lbs)
varImpPlot(hlbt_lbs)
hlbt_lbs$importance
Imp_Var(hlbt_lbs) # get important variables
# important variables below
partialPlot(hlbt_lbs,CPD_Hal,AnnChl)
partialPlot(hlbt_lbs,CPD_Hal,log_Poll_Age1_recruits_millions)
partialPlot(hlbt_lbs,CPD_Hal,TotTCrab)
partialPlot(hlbt_lbs,CPD_Hal,log_Pink_Shrimp)
partialPlot(hlbt_lbs,CPD_Hal,hlbt_real_rev)








