###############################################
### DMX Linkages Group 
### Random Forest Analysis Script
### Started by Rachael Blake    January 2016
###############################################

# load necessary packages
library(randomForest)
library(plyr)
library(dplyr)



# call the data assembly script
source("commPracticeDataFormat.R")
CPD <- CoPrct

# filter data for complete cases
#CPD2 <- CPD %>%
#        filter(complete.cases(.))

#CPD3 <- CPD %>%
#        select(-SkateBiomass,-SharkAbundIPHC,-SSLnonPup_anul_mn,-WndDir_degT_Winter,-WndSp_m_s_Winter) %>%
#        filter(complete.cases(.))

CPD4 <- CPD %>%
        select(which(colMeans(is.na(.)) < 0.5)) %>%
        filter(complete.cases(.))


###
# Pink Salmon
goaPink <- randomForest(goaPinkCatchNum ~., data=CPD4, importance=T, do.trace=1000, ntree=5000)
print(goaPink)
plot(goaPink)
varImpPlot(goaPink)
goaPink$importance

# Arrowtooth
arrow <- randomForest(ArrAdult ~., data=CPD4, importance=T, do.trace=1000, ntree=5000)
print(arrow)
plot(arrow)
varImpPlot(arrow)
arrow$importance

# Pollock Adults
poll_a <- randomForest(Poll_Yr3plus_TtlBmss_1000Tons ~., data=CPD4, importance=T, do.trace=1000, ntree=5000)
print(poll_a)
plot(poll_a)
varImpPlot(poll_a)
poll_a$importance

# Pollock Juvenile
poll_j <- randomForest(Poll_Age1_recruits_millions ~., data=CPD4, importance=T, do.trace=1000, ntree=5000)
print(poll_j)
plot(poll_j)
varImpPlot(poll_j)
poll_j$importance

# Halibut
hlbt_lbs <- randomForest(hlbt_pounds ~., data=CPD4, importance=T, do.trace=1000, ntree=5000)
print(hlbt_lbs)
plot(hlbt_lbs)
varImpPlot(hlbt_lbs)
hlbt_lbs$importance





# build function to run random forests on each variable(column)
RF_per_var <- function(df){
              # insert code to make it do it for every column, but not overwrite
              for(i in ncol(df)){
                  rf <- randomForest(i ~., data=df, importance=T, do.trace=1000, ntree=5000)
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
