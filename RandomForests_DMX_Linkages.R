###############################################
### DMX Linkages Group 
### Random Forest Analysis Script
### Started by Rachael Blake    January 2016
###############################################

library(randomForest);library(plyr);library(dplyr)

# call the data assembly script

source("commPracticeDataFormat.R")
CPD <- CoPrct

# filter data for complete cases

#CPD4 <- CPD %>%
#        select(which(colMeans(is.na(.)) < 0.5)) %>%
#        filter(complete.cases(.))

####
# FUNCTION to run randomForest analysis and print outputs and plots
####
RForestRun <- function(subset_df, resp_column){
              a <- randomForest(resp_column ~., data=subset_df, importance=T, do.trace=1000, ntree=5000)
              b <- print(a)   
              c <- plot(a)
              d <- varImpPlot(a)
              e <- a$importance
              rflist <- c(a,b,c,d,e)
              return(rflist)
              }


####
####



###
# Pink Salmon
CPD_Pink <- CPD %>% 
            select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                   WTemp_C_AnnMn,AnnChl,SewardLineMayEuphausiids,SewardLineMayCopepods,
                   goaPinkCatchNum,PWS_WildPinkSalmon_SSB_ModelOutput,ArrAdult,
                   Poll_Yr3plus_TtlBmss_1000Tons) %>%
            filter(complete.cases(.))
goaPink <- randomForest(PWS_WildPinkSalmon_SSB_ModelOutput ~., data=CPD_Pink, importance=T, do.trace=1000, ntree=5000)
print(goaPink)
plot(goaPink)
varImpPlot(goaPink)
goaPink$importance

# Arrowtooth   ## NOTE: Capelin left out due to too few data points
CPD_Arrow <- CPD %>% 
             select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                    WTemp_C_AnnMn,AnnChl,SewardLineMayEuphausiids,Poll_Age1_recruits_millions,
                    ArrAdult,arth_tons,arth_real_rev,arth_vessels,arth_processors,arth_real_price) %>%
             filter(complete.cases(.))
arrow <- randomForest(ArrAdult ~., data=CPD_Arrow, importance=T, do.trace=1000, ntree=5000)
print(arrow)
plot(arrow)
varImpPlot(arrow)
arrow$importance



# Pollock Adults
CPD_Poll <- CPD %>% 
            select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                   WTemp_C_AnnMn,AnnChl,EKE_ann_max_mean,SewardLineMayEuphausiids,
                   SewardLineMayCopepods,Pink_Shrimp,Poll_Age1_recruits_millions,
                   Poll_Yr3plus_TtlBmss_1000Tons,ArrAdult, PCod_female_Bmss_t,hlbt_pounds,
                   plck_tons,plck_real_rev,plck_vessels,plck_processors) %>%
            filter(complete.cases(.))
poll_a <- randomForest(Poll_Yr3plus_TtlBmss_1000Tons ~., data=CPD_Poll, importance=T, do.trace=1000, ntree=5000)
print(poll_a)
plot(poll_a)
varImpPlot(poll_a)
poll_a$importance



# Pollock Juvenile
CPD_JPoll <- CPD %>%
             select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                    WTemp_C_AnnMn,AnnChl, EKE_ann_max_mean,SewardLineMayEuphausiids,
                    SewardLineMayCopepods,Poll_Age1_recruits_millions,
                    Poll_Yr3plus_TtlBmss_1000Tons,ArrAdult) %>%
             filter(complete.cases(.))
poll_j <- randomForest(Poll_Age1_recruits_millions ~., data=CPD_JPoll, importance=T, do.trace=1000, ntree=5000)
print(poll_j)
plot(poll_j)
varImpPlot(poll_j)
poll_j$importance



# Halibut
CPD_Hal <- CPD %>% 
           select(PDO_anul_mn,NPGO_anul_mn,#WndSp_m_s_AnnMn,WndDir_degT_AnnMn,
                  WTemp_C_AnnMn,AnnChl,Poll_Yr3plus_TtlBmss_1000Tons, Poll_Age1_recruits_millions,
                  TotTCrab,Pink_Shrimp,hlbt_pounds,hlbt_real_rev,hlbt_vessels,hlbt_processors) %>%
           filter(complete.cases(.))
       
hlbt_lbs <- randomForest(hlbt_pounds ~., data=CPD_Hal, importance=T, do.trace=1000, ntree=5000)
print(hlbt_lbs)
plot(hlbt_lbs)
varImpPlot(hlbt_lbs)
hlbt_lbs$importance











# load necessary packages

# build function to run random forests on each variable
RF_per_var <- function(df){
              # insert code to make it do it for every column, but not overwrite
              for("everyColumn" in df){
                  rf <- randomForest(variable ~., data=df, importance=T, do.trace=1000, ntree=5000)
                  return(rf)
              }
              }

#####################################################




### Epiphytic Chla
rf <- randomForest(EpiChla ~., data=FS08RF, importance=T, do.trace=1000, ntree=5000)

print(rf)
plot(rf)  # to assess the number of trees to build by looking at amount of error
table(predict(rf), FS08RF$EpiChla)  # How well are we predicting?
# This gives an error saying "all arguments must have the same length"...?????


varImpPlot(rf)  # Dot plots of importance.
rf$importance    # lists the actual importance values
# Strobl & Zeileis say don't use the Z score thing below, which divides the MSE by the "standard error".
#importance(rf, type=1, scale=T) # Gives a Scale Importance value (z score); see Strobl & Zeileis PP presentation
#write.table(rf$importance, file="Imp_EpiChl.csv")

epi <- read.csv("Imp_EpiChl.csv")
dotchart(epi[,2], labels=mse[,1])


# Partial dependence plots
# X-axis is value of the predictor, y-axis is prediction averaged over all trees in the forest
partialPlot(rf,FS08,ZosLeafPropN, ylim=c(0,2.5),xlim=c(0,21))
partialPlot(rf,FS08,Salinity,col="red", add=T) # add another variable to the plot
partialPlot(rf,FS08,WaveExp,col="blue",add=T)
partialPlot(rf,FS08,GzrBmss_Norm_g,col="green",add=T)

## Suggestions from John Hoenig...Feb 21, 2012
# try plotting predicted (on y axis) vs. observed  or residuals vs. observed 
# also look at r2 for difference between predicted and observed.

names(FS08RF)
### Seagrass Biomass
rf2 <- randomForest(Seagrass_Bmss_Norm ~., data=FS08RF, importance=T, do.trace=1000, ntree=5000)
print(rf2)
plot(rf2)  # to assess the number of trees to build by looking at amount of error
table(predict(rf2), FS08RF$Seagrass_Bmss_Norm)  
varImpPlot(rf2)  # Dot plots of importance.
rf2$importance    # lists the actual importance values
write.table(rf2$importance, file="Imp_GrassBmss.csv")
sgbmss <- read.csv("Imp_GrassBmss.csv")

partialPlot(rf,FS08,PropDevelShoreline, ylim=c(1,2),xlim=c(0,80))
partialPlot(rf,FS08,GrassCover,col="red", add=T) # add another variable to the plot
partialPlot(rf,FS08,WaveExp,col="blue",add=T)
partialPlot(rf,FS08,GzrBmss_Norm_g,col="green",add=T)
partialPlot(rf,FS08,WaterTemp,col="orange",add=T)
