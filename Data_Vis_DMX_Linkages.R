#################################################
### DMX Linkages
### Data Visualizations
### Script by Rachael Blake   January 2016
#################################################

# load necessary packages
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(scales)

# call the data assembly script
source("commPracticeDataFormat.R")
head(CoPrct) ; str(CoPrct)


# plot data presence in time for each variable
melt_CoPr <- melt(CoPrct, id.vars="Year", variable_name="Data_Set")

melt_CoPr <- mutate(melt_CoPr, Bin_Value = ifelse((is.na(value)),'0',
                                           ifelse((!is.na(value)),'1',"")))


p <- ggplot(data=melt_CoPr, aes(x=Data_Set, y=Year)) + 
            geom_tile(aes(fill = Bin_Value), colour = "white") +
            scale_fill_manual(values=c("0"="white", "1"="red"), guide=FALSE) +
            scale_y_reverse() + 
            theme(axis.text.x = element_text(angle=90, vjust=0.3, hjust=1, color="black", size=15),
                  axis.text.y = element_text(color="black", size=15),
                  axis.title  = element_text(face="bold", size=20))
p









