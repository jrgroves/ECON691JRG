library(tidycensus)
library(tidyverse)
library(sf)

#Functions
perc<-function(x,y){ 
  temp<-x/y 
  return(temp)
}

#Generate data from Census API
#Pre-defining variables to be used in loop

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003","B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )
states<-c("kentucky","indiana","illinois","missouri","wisconsin","iowa") 
fips<-c(21,18,17,29,55,19)

#API Command
k<-1

for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars, 
               state = i,
               year  =  2016, 
               geometry  =  TRUE)
  
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map"),map) 
  k<-k+1
  rm(temp, map)
}

census<-rbind(illinoiscensus,iowacensus,kentuckycensus,wisconsincensus,
              missouricensus,indianacensus) 
States<-rbind(illinoismap,iowamap,kentuckymap,wisconsinmap,missourimap,
              indianamap)

census$NAME<-as.data.frame(str_split_fixed(census$NAME, ",", 2))[,1] 
  census$NAME<-trimws(gsub(" County","",census$NAME))

#Load Vote Data 
  load("./Build/Output/votes.RData")
  
  votes$County[which(votes$County=="DeWitt")]<-"De  Witt"
  votes$County[which(votes$County=="JoDaviess")]<-"Jo Daviess" 
  votes$County[which(votes$County=="LaClede")]<-"Laclede" 
  votes$County[which(votes$County=="LaRue")]<-"Larue" 
  votes$County[which(votes$County=="St. Louis City")]<-"St. Louis city" 
  census$NAME[which(census$NAME=="St. Louis")]<-"St. Louis County"
  
#Merge vote  and  census  data
  core<-merge(census,votes,by.x=c("NAME","state"), by.y=c("County","state"),all=TRUE)
  core$area<-st_area(core) #Command for maps in ggplot later

  