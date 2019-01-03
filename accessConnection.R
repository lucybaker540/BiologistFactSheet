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

Stationsquery<- sqlQuery(channel, 'SELECT * FROM Stations')

totalHab <- sqlQuery(channel,'SELECT * FROM HabValues')

totHabCon <- sqlQuery(channel,'SELECT * FROM HabSamps')
# Lucy: pull in Stations table and practice joining to other tables

## Lucy assignment:
#function to filter by 1 station
# lucy stretch: filter above by date rance, or year(s)

#Select Station from stations query to add stream name and location info to markdown
UserStation<-"4ASRE022.30"
startyear<- 2008
endyear<- 2014


stationqry<- function(UserStation){
  filter(Stationsquery,StationID==UserStation)
}
StationQRY<-stationqry(UserStation)

#Function to filter by 1 station and range of years



selectStation<-function(UserStation, startyear, endyear){
  station<- filter(SCIquery,StationID==UserStation)
  station<-subset(station, RepNum==1)#only selects Rep 1's
  subset(station, Year>= as.integer(startyear) & Year<= as.integer(endyear))

  }


annualdata<-selectStation(UserStation, startyear, endyear)

#Display impaired or non-impaired message

VSCItester <- function(VSCI){
  
  message1 <- NA
  
  for (i in 1:length(VSCI)){
    
    if(is.na(VSCI[i])){
      message1[i] <- 'not sampled'
    }else{
      if(VSCI[i] >= 60){message1[i] <- 'Not Impaired'}
      if(VSCI[i] < 60){message1[i] <- 'Impaired'}
    }
    
  }
  return(message1)
  
}

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
  datatable(tableData, rownames=F, options = list(scrollY = '500px', pageLength= 10, dom = 't')) %>%
    formatStyle('Fall Assessment', textAlign = 'center') %>%
    formatStyle('Spring Assessment', textAlign = 'center')
  
}

basicTable(annualdata)


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



# plot seasonal/annual total hab scores


#function to select station and years from habitat query
selecthabitatStation<-function(UserStation, startyear,endyear){
  station<- filter(totalHabitat,StationID==UserStation)
  date1<-as.Date(paste(startyear, 01, 01, sep = "-"))
  date2<-as.Date(paste(endyear, 01, 01, sep = "-"))
  subset(station, CollDate>= date1 & CollDate<= date2)
}

hab<-selecthabitatStation(UserStation, startyear, endyear)

totHab <- hab %>%
  group_by(StationID, CollDate ) %>%
  mutate(TotalHabitat = sum(HabValue))
totHab$CollDate <- as.Date(as.character(totHab$CollDate),format="%Y-%m-%d")


####totHabWide <- select(totHab, StationID, CollDate, `Total Habitat`, HabParameter, HabValue) %>%
 ## spread(HabParameter,HabValue)
# make table of high/low gradient variables:
# in BenthicStressorReportTemplate_VSCI.Rmd ctrl+F habData ~line 611


HabitatDT<- function(totHab){
if("POOLSUB" %in% unique(totHab$HabParameter) | 'POOLVAR' %in% unique(totHab$HabParameter)){ 
  # Low Gradient Habitat Method
  totHab$CollDate <- as.Date(as.character(totHab$CollDate),format="%Y-%m-%d")
  totHabData <- select(totHab,StationID,CollDate,TotalHabitat)[!duplicated(select(totHab,StationID,CollDate,TotalHabitat)),]# get unique sample dates
  if(!"SUBSTRATE" %in% names(totHab$HabParameter)){
    totHab$SUBSTRATE<-0 
  }
  habData1 <- select(totHab,-c(TotalHabitat, HabSampID, Comments, EnterDate,HSampIndex, CollTime,`Field Team`)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
    filter(HabParameter %in% c("ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                               "SUBSTRATE","COVER")) %>% # get only low gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
    spread(HabParameter,HabValue) %>%
    filter(!is.na(POOLSUB) & !is.na(POOLVAR) & !is.na(SINUOSITY)) # if both methods used then get rid of sample dates not sampled as low gradient

  habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                          "SUBSTRATE","COVER")]
  # Deal with COVER terminology if present
  if("COVER" %in% names(habData1)){
    habData1 <- habData1 %>% rowwise()%>%
      mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
      left_join(totHabData, by=c("StationID","CollDate")) %>%
      dplyr::rename(TotalHabitat = TotalHabitat) %>%
      arrange(CollDate)
    habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                            "SUBSTRATE","TotalHabitat")]
    }else{
    habData1 <- habData1 %>% rowwise()%>%
      left_join(totHabData, by=c("StationID","CollDate")) %>%
      dplyr::arrange(CollDate)
    habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                            "SUBSTRATE","TotalHabitat")]
  }
  DT::datatable(habData1,escape=F, rownames = F, 
                colnames = c('Station ID','Date','Channel Alteration','Bank Stability','Bank Vegetation', 
                             'Flow', 'Pool Substrate','Pool Variability', 'Riparian Vegetation', 
                             'Sediment', 'Sinuosity', 'Substrate', 'Total Habitat'),
                options=list(pageLength=nrow(habData1),dom= 'Bt' )) %>%
    formatStyle(names(habData1)[3:12], textAlign = 'center', `font-family` = 'Arial') %>%
    formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal')), 
                textAlign = 'center', `font-family` = 'Arial') %>%
    formatStyle('TotalHabitat', backgroundColor = "lightgray")
}

if("RIFFLES" %in% unique(totHab$HabParameter) | "VELOCITY" %in% unique(totHab$HabParameter) ){ # High Gradient Habitat Method
  totHab$CollDate <- as.Date(as.character(totHab$CollDate),format="%Y-%m-%d")
  totHabData <- select(totHab,StationID,CollDate,TotalHabitat)[!duplicated(select(totHab,StationID,CollDate,TotalHabitat)),] # get unique sample dates
    if(!"SUBSTRATE" %in% names(totHab)){
    totHab$SUBSTRATE<-0 
    }
  habData1 <- select(totHab,-c(TotalHabitat,HabSampID,Comments, EnterDate,HSampIndex, CollTime,`Field Team`)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
    filter(HabParameter %in% c('ALTER','BANKS','BANKVEG','COVER','EMBED','FLOW','RIFFLES','RIPVEG','SEDIMENT',
                               'SUBSTRATE','VELOCITY')) %>% # get only high gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
    spread(HabParameter,HabValue) %>%
    filter(!is.na(EMBED) & !is.na(RIFFLES) & !is.na(VELOCITY)) # if both methods used then get rid of sample dates not sampled as high gradient
  
  # Deal with COVER terminology if present
  if("COVER" %in% names(habData1)){
    habData1 <- habData1 %>% rowwise()%>%
      mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
      left_join(totHabData, by=c("StationID","CollDate")) %>%
      dplyr::arrange(CollDate)
    habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG", "EMBED", "FLOW","RIFFLES","RIPVEG","SEDIMENT","SUBSTRATE",
                            "VELOCITY","TotalHabitat")]
  }else{
    habData1 <- habData1 %>% rowwise()%>%
      left_join(totHabData, by=c("StationID","CollDate")) %>%
      dplyr::arrange(CollDate)
    habData1<-habData1[ , c("StationID", "CollDate", "ALTER","BANKS","BANKVEG", "EMBED", "FLOW","RIFFLES","RIPVEG","SEDIMENT","SUBSTRATE",
                            "VELOCITY","TotalHabitat")]
  }
  DT::datatable(habData1,escape=F, rownames = F, colnames = c('Station ID','Date','Channel Alteration','Banks', 
                                                              'Bank Vegetation', 'Embeddedness', 
                                                              'Flow', 'Riffles', 'Riparian Vegetation', 
                                                              'Sediment', 'Substrate','Velocity', 'Total Habitat'),
                options=list(pageLength=nrow(habData1),dom= 'Bt' )) %>%
    formatStyle(names(habData1)[4:12],  textAlign = 'center', `font-family` = 'Arial') %>%
    formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal')), 
                textAlign = 'center', `font-family` = 'Arial') %>%
    formatStyle('TotalHabitat', backgroundColor = "lightgray")
}
}



HabitatDT(totHab)


as.POSIXct(totHab$CollDate)
habitatplot<-function(totHab) {
  hab<- as.Date(totHab$CollDate,format="%Y-%m-%d")
  ggplot(hab, aes(x=CollDate , y=TotalHabitat))+
    geom_col()+
    labs(x="Collection Date", y="Total Habitat")+
    theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),
          legend.title = element_text(size=14, face="bold"),
          axis.title=element_text(size=14, face="bold")) +
    scale_y_continuous(name="Total Habitat", breaks=seq(0, 200, 25),limits=c(0,200))+
    scale_x_datetime(date_breaks='1 year')+
    theme(axis.text.x=element_text(angle=45,hjust=1))
    
}

habitatplot(totHab)
