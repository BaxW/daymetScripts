########## This script is an example of how I would download climatic data from daymet and claculate cummulative growing degree days (cGDD) over the first few months of the year for multiple locations/ years
##BWW

##### Packages 

library(tidyverse)
library(daymetr)


# corrdinates for 3 arbitrary points somewhere in North America

points <- tibble(ID=LETTERS[1:3], lat=runif(3,35,50), lon=runif(3,-120,-85))

# save points as csv 
write.csv(points, "sites.csv", row.names = F)

# download daymet data for just 3 years (into memory)
# note that daymet data is available back to 1985
daymetDat <- download_daymet_batch(file_location = "sites.csv",
                                   start = 2012,
                                   end = 2014,
                                   internal = T)

# need helper function to pull data out of this list and store it as a long DF


datGet <- function(x) {
  dat <- x$data
  ID <- x$site
  elev <- x$altitude
data.frame(ID = rep(ID, nrow(dat)), elev=rep(elev, nrow(dat)), dat) %>%
  mutate(ID=as.character(ID))
  
}

# apply helper function to the list of daymet data with purrr


full_daymetDF <- map_df(daymetDat, datGet)


# fix the long var names

names(full_daymetDF) <- c("ID", "elev", "year", "doy", "dayl", "prcp", "srad", "swe" ,"tmax", "tmin","vp")


# remove now redundant list
rm(daymetDat)


# Use years and IDs as grouping varaibles to calculate cGDD across all years and locations in the dataframe
# I calcualte for the period Jan 1st - April 1st 
# I use Tbase of 0 degreesC

cGDDapril <- full_daymetDF %>%
  filter(doy<=91) %>% # april 1st is the 91st day of the year
  group_by(ID,year) %>%
  mutate(GD=ifelse(tmax<=0,0,(tmax-tmin)/2 - 0)) %>% # calculate GDD for each day
  summarize(cGDD= sum(GD)) # calcualte cGDD as the sum of GDD before april 1st

# plot 

ggplot(cGDDapril, aes(x=as.factor(year), y=cGDD, fill=ID)) +
  geom_jitter(shape = 21, color = "black", size = 2, stroke = .5, alpha= .75, width = .13) +
  labs(y="cGDD", x= "Year", title = "cGDD by April First")


# compare accumulation curves for a given year

# helper fuction to convert a column of daily cGDD values to a cumulative sum 

helpr <- function(x){tibble(doy=x$doy,GDaccumulated=cumsum(x$GD))}


dat2012 <- full_daymetDF %>%
  filter(year==2012, doy<=91) %>% #subset Jan1st-april1st 2012
  group_by(ID) %>%
  arrange(doy) %>% # make sure it's in chronological order
  mutate(GD=ifelse(tmax<=0,0,(tmax-tmin)/2 - 0)) %>% # calcualte growing degrees for each day
  group_modify(~helpr(.x))  # apply helper function to get the GD accumulation curve



# plot

ggplot(dat2012, aes(x=doy, y=GDaccumulated, group=ID, color=ID)) +
  geom_path() +
  labs(x= "Day of Year", y= "cGDD")






