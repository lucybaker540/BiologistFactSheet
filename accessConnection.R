# R 3.5.1

# This file walks you through connecting MS Access to R
# MS Access is dumb and needs to be run in 32bit R

# packages
library(tidyverse)
library(RODBC)
library(dplyr)
library(DT)
library(ggplot2)
library(scales)


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

UserStation<-"4ASRE022.30"
startyear<- 2006
endyear<- 2014

selectStation<-function(UserStation, startyear, endyear){
  station<- filter(SCIquery,StationID==UserStation)
  subset(station, Year>= as.integer(startyear) & Year<= as.integer(endyear))
  subset(station, RepNum==1)#only selects Rep 1's
  }


annualdata<-selectStation(UserStation, startyear, endyear)

#Table function for VSCI metrics
basicTable <- function(annualdata){
  # grab info we want from input dataframe
  tableData <- select(annualdata, Year, Season, IBI) %>%
    spread(Season, IBI)%>%
    mutate(Fall = format(Fall, digits=3),
           `Fall Assessment` = VSCItester(Fall),
           Spring = format(Spring, digits=3),
           `Spring Assessment` = VSCItester(Spring)) %>%
    select(Year, Fall, `Fall Assessment`, Spring, `Spring Assessment`)
  #make table the way we want
  datatable(tableData, rownames=F, options = list(scrollY = '200px', pageLength= 6, dom = 't')) %>%
    formatStyle('Fall Assessment', textAlign = 'center') %>%
    formatStyle('Spring Assessment', textAlign = 'center')
  
}

basicTable(annualdata)

VSCItester <- function(VSCI){
  
  message1 <- NA
  
  for (i in 1:length(VSCI)){
    
    if(is.na(VSCI[i]) | is.null(VSCI[i])){
      message1[i] <- 'not sampled'
    }else{
      if(VSCI[i] >= 60){message1[i] <- 'Not Impaired'}
      if(VSCI[i] < 60){message1[i] <- 'Impaired'}
    }
    
  }
  return(message1)
  
}

#Make a plot function to plot yearly VSCI scores- stole code from stressor report code, this won't plot data but I'm not sure why not! 

bugplot<-function(annualdata) {
  ggplot(annualdata, aes(x=CollDate, y=IBI, fill=Season))+
  geom_col()+
  scale_fill_manual("Season", values = c("Fall" = "black", "Spring" = "dark grey"))+
  labs(x="Collection Date", y="VSCI")+
  theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),
        legend.title = element_text(size=14, face="bold"),
        axis.title=element_text(size=14, face="bold")) +
  scale_y_continuous(name="VSCI", breaks=seq(0, 100, 10),limits=c(0,100))+
  scale_x_datetime(date_breaks='1 year')+
  geom_hline(yintercept=60, color="red", size=1)+
  theme(axis.text.x=element_text(angle=45,hjust=1))

}

bugplot(annualdata)

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
#function to select station and years from habitat query
selecthabitatStation<-function(UserStation, startyear,endyear){
  station<- filter(totHabWide,StationID==UserStation)
  date1<-as.Date(paste(startyear, 01, 01, sep = "-"))
  date2<-as.Date(paste(endyear, 01, 01, sep = "-"))
  subset(station, CollDate>= date1 & CollDate<= date2)
}

hab<-selecthabitatStation(UserStation, startyear, endyear)

# make table of high/low gradient variables:
# in BenthicStressorReportTemplate_VSCI.Rmd ctrl+F habData ~line 611