#This is a script to download the election results by county for the 
#2016 US Presidential Election, download Census data via API, and use 
#ggplot to create state maps

#Created by J. R. Groves on August 19, 2021

rm(list=ls())

library(rvest) #rvest is used to scrape the New York Times website for the needed data.
library(tidyverse)

#Specifying the url for desired website to be scraped

url <- "https://www.nytimes.com/elections/2016/results/illinois"

#Reading the HTML code from the website

webpage <- read_html(url)

tables<-webpage %>%
    html_nodes("table") #This pulls out all the "table" nodes in the HTML code

#Nested and pipped version of the command to extract the correct table #and turn #into a dataframe.

results<-as.data.frame(html_table(tables[2],fill=TRUE,header=TRUE)) 

results2<-tables[2] %>%
  html_table(fill=TRUE,header=TRUE) %>% 
  as.data.frame()
head(results)

Illinois<-results2 %>%
  rename("County" = "Vote.by.county") %>%
  mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
         "Trump" = as.numeric(gsub(",","",Trump)),
         "pctClinton" = (Clinton)/(Clinton+Trump),
         "pctTrump" = Trump/(Clinton+Trump))

#List of the states the data will be pulled for states

states<-c("kentucky","indiana","illinois","missouri","wisconsin","iowa")

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
           "pctTrump" = Trump/(Clinton+Trump))
  assign(i,results2)
}

library(tidycensus)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")

acs <- get_acs(geography = "county",	#defines geography level of data 
               variables = vars,	    #specifics the data we want 
               state = 17,	          #denotes the specific state 
               year = 2016,	          #denotes the year
               geometry = TRUE)	      #downloads the TIGER shapefile data  

il.acs<-acs %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

##### Other APIs ####
library(httr) 
library(jsonlite)

covid<-GET("https://data.cdc.gov/resource/n8mc-b4w4.json")

covid.2<-fromJSON(rawToChar(covid$content))

head(covid.2)

##### Plot with ggplot2####

ggplot(il.acs) +
  geom_sf(aes(fill = perMale))

il.acs$num<-seq(1:102)

ggplot(il.acs, aes(x=num))+
  geom_line(aes(y=perMale), color="red")+
  geom_point(aes(y=perWhite), color="blue")
  
ggplot(il.acs, aes(x=num))+
  geom_line(aes(y=perMale, color="red"))+
  geom_point(aes(y=perWhite, color="blue"))+
  xlab("County")+
  ylab("Percentage")+
  ggtitle("Percent of Population by County in Illinois")+
  scale_color_manual(name="Percent of",
                     breaks = c("red","blue"),
                     values  =  c("red","blue"),
                     labels = c("Male","White"))

#Remove the added text in ACS data
  il.acs$County<-trimws(gsub(" County, Illinois","",il.acs$NAME))
            
#Sort both data so they have the same sorting process
  il.acs<-il.acs[order(il.acs$County),]
  
  illinois<-illinois[order(illinois$County),]
#Logic test to make sure the names match
  
  il.acs$County==illinois$County
  
  il.acs$County[19:20]
  
  illinois$County[19:20]
  
  il.acs$County[19]<-illinois$County[20]
  il.acs$County[43]<-illinois$County[43]

il.acs<-merge(il.acs,illinois,by="County",all=TRUE) 
  
b<-il.acs[is.na(il.acs$perMale),]
  
il.acs<-il.acs[!is.na(il.acs$perMale),]
  
ggplot(il.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))
  
library(cowplot)

p1<-ggplot(il.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

p2<-ggplot(il.acs)+ 
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent  White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1,p2)

