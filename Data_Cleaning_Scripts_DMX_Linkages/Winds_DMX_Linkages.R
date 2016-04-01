###########################################################
##### Data Cleaning Script - DMX Linkages
#####  Winds (annual and winter) : from National Buoy Data Center
###########################################################

## load packages (order matters)
library(httr)
library(plyr)
library(dplyr)
library(XML)
library(curl)
library(rvest)
library(tidyr)
library(stringr)

## Steps for data cleaning: 
## 1) read in data
## 2) format to annual estimates (2 column dataframe with cols=Year,spEstimate)

#############
# Winds (annual and winter) : from National Buoy Data Center

# selected buoys in central Gulf of Alaska:
# Station 46076  NDBC  CAPE CLEARE - 17 NM South of Montague Is, AK
      # NOTE: The 2005 data starts in June
# Station 46080  NDBC  PORTLOCK BANK- 76NM ENE of Kodiak, AK
      # NOTE: Data starts in August of 2002
      # NOTE: 2013 data missing from online http://www.ndbc.noaa.gov/station_history.php?station=46080
# Station 46078  NDBC  ALBATROSS BANK - 104NM South of Kodiak, AK
      # NOTE: Data starts in May 2004


#############
# Function to read in all these annual bouy data text files, and make data frames of them.
BuoyData <- function(data_url){
            dataGet <- GET(data_url)
            data1 <- content(dataGet, as='text')

            # need to say get year from URL
            year <- as.numeric(str_sub(data_url,60,63))
            # get buoy number from URL
            buoynum <- str_sub(data_url,54,58)

            # need to say if year < 2007 do this...
            if (year < 2007) {
                              df <- read.table(file=textConnection(data1),
                                               stringsAsFactors=FALSE,header=TRUE)
                              df$BuoyID <- rep(buoynum,nrow(df))
                              }
            # and if year >/= 2007 do this...
            else {
                  data_h <- scan(textConnection(data1), nlines=1, what=character())  # reads first header line
                  data_h <- gsub("#YY", "YYYY", data_h)  # gets rid of column name with # in it
                  df <- read.table(file=textConnection(data1),
                                   stringsAsFactors=FALSE,skip=2,header=FALSE)
                  names(df) <- data_h   # pastes the header line in
                  df$BuoyID <- rep(buoynum,nrow(df))
                  df <- rename(df, WD=WDIR, BAR=PRES)
                  }

            return(df)
            }
#############

# List of URLs from which to pull the text files
B_URLs <- list("http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2005.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2006.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2007.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2008.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2009.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2010.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2011.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2012.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2013.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46076h2014.txt.gz&dir=data/historical/stdmet/",
               #
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2002.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2003.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2004.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2005.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2006.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2007.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2008.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2009.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2010.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2011.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2012.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46080h2014.txt.gz&dir=data/historical/stdmet/",
               #
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2004.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2005.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2006.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2007.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2008.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2009.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2010.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2011.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2012.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2013.txt.gz&dir=data/historical/stdmet/",
               "http://www.ndbc.noaa.gov/view_text_file.php?filename=46078h2014.txt.gz&dir=data/historical/stdmet/"
               ) 


Buoy_df_list <- lapply(B_URLs, FUN=BuoyData) # for every element of the list of URLs run my function

Buoys_all <- bind_rows(Buoy_df_list) # bind the list of dataframes output by lapply() into one large dataframe


#
Wind_Ann <- Buoys_all %>%
                  filter(WD!=99, WD!=999, WSPD!=99, WSPD!=999) %>%  # remove missing data
                  rename(Year=YYYY) %>%       # rename column for uniformity
                  group_by(Year) %>%
                  summarise(WndSp_m_s_AnnMn=mean(WSPD)) %>%  # get means
                  ungroup() %>%
                  select(Year, WndSp_m_s_AnnMn) # select wind speed

Wind_Winter <- Buoys_all %>%
                     filter(WD!=99, WD!=999, WSPD!=99, WSPD!=999) %>%  # remove missing data
                     rename(Year=YYYY) %>%       # rename column for uniformity
                     filter(MM %in% c(12,1,2)) %>%
                     group_by(Year) %>%
                     summarise(WndSp_m_s_Winter=mean(WSPD)) %>%  # get means
                     ungroup() %>%
                     select(Year, WndSp_m_s_Winter)  # select wind speed
#