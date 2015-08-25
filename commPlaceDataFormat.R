## Communities of Place Data compilation
## created 24 Aug 2015
## Add data to 

library(dplyr)

CoPlc=data.frame(
  'year'=c(1975:2015))

### bring in data, format to annual estimates (2 col df with cols=year,spEstimate) and merge with CoPlc dataframe
# CoPlc=merge(CoPlc,newData,all.x=T) ## rename new df CoPlc