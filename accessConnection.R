# R 3.5.1

# This file walks you through connecting MS Access to R
# MS Access is dumb and needs to be run in 32bit R

# packages
library(tidyverse)
library(RODBC)


# Make connection with Access
channel <- odbcConnectAccess2007('data/EDASxp_Family_090117.accdb')

# Pull tables you need
SCIquery <- sqlQuery(channel, 'SELECT * FROM SCIQuery')

totalHab <- sqlQuery(channel,'SELECT * FROM HabValues')

totHabCon <- sqlQuery(channel,'SELECT * FROM HabSamps')
# Lucy: pull in Stations table and practice joining to other tables

## Lucy assignment:
#function to filter by 1 station
# lucy stretch: filter above by date rance, or year(s)

#Function to filter by 1 station and range of years

selectStation<-function(UserStation, startyear, endyear){
  station<- filter(SCIquery,StationID==UserStation)
  subset(station, Year>= as.integer(startyear) & Year<= as.integer(endyear))
  
}

UserStation<-"1ACUB004.63"
startyear<- as.integer(2008)
endyear<- as.integer(2014)
df<-selectStation(UserStation, startyear, endyear)


# Total hab mess around
totalHabitat <- left_join(totalHab,totHabCon, by=c('HabSampID', 'Comments','EnterDate'))

rm(totHabCon); rm(totalHab)
# test manipulation zone

totHab <- totalHabitat %>%
  group_by(HabSampID, CollDate, StationID) %>%
  mutate(`Total Habitat` = sum(HabValue))

# plot seasonal/annual total hab scores

totHabWide <- select(totHab, StationID, CollDate, `Total Habitat`, HabParameter, HabValue) %>%
  spread(HabParameter,HabValue)

# make table of high/low gradient variables:
# in BenthicStressorReportTemplate_VSCI.Rmd ctrl+F habData ~line 611