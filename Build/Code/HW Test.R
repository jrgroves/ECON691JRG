rm(list=ls())

library(tidycensus)
library(tidyverse)
library(sf)



#Functions


#Generate data from Census API
#Pre-defining variables to be used in loop

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003","B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )
states<-c("colorado","wyoming","utah","new-mexico","nebraska") 
fips<-c(08,56,49,35,31)

k<-1
for(i in states){
  #Specifying the URL for desired website to be scraped
  url.1 <- "https://www.nytimes.com/elections/2016/results/"
  
  url<-paste0(url.1,i)
  webpage <- read_html(url)
  tables<-webpage %>%
    html_nodes("table") #This pulls out all the "table" nodes in the HTML code
  
  results2<-tables[2] %>%
    html_table(fill=TRUE,header=TRUE) %>% 
    as.data.frame() %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton" = (Clinton)/(Clinton+Trump),
           "pctTrump" = Trump/(Clinton+Trump),
           state = i)
  ifelse(k==1,votes<-results2,votes<-rbind(votes,results2))
  k<-k+1
}

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
  
  ifelse(k==1,census<-temp,census<-rbind(census, temp))
  
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  ifelse(k==1,stmap<-map,stmap<-rbind(stmap,map))
  k<-k+1
  rm(temp, map)
}

census$NAME<-as.data.frame(str_split_fixed(census$NAME, ",", 2))[,1] 
  census$NAME<-trimws(gsub(" County","",census$NAME))

  core<-merge(census,votes,by.x=c("NAME","state"), by.y=c("County","state"),all=TRUE)
  
#Merge vote  and  census  data

  core$area<-st_area(core) #Command for maps in ggplot later

  core<-core %>%
    subset(!is.na(perWhite))

#Map Full Core Data#####
  
  ggplot(core)+ 
    geom_sf(aes(fill = perWhite))+
    scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent  White"))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank())
   
  
  ggplot(core)+ 
    geom_sf(aes(fill = pctClinton))+
    scale_fill_gradient(low="red",high="blue",limits=c(0,1),
                         aes(name="Percent Clinton"))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank()) 
  