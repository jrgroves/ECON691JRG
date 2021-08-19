library(rvest)
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
                                                 
#List of the states the data will be pulled for
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
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )
#Command to pull data from ACS
acs <- get_acs(geography = "county",	#defines geography level of data 
               variables = vars,	#specifics  the  data  we  want 
               state  =  17,	#denotes  the  specific  state 
               year=2016,	#denotes  the  year
               geometry  =  TRUE)	#downloads  the  TIGER  shapefile  data

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
  spread(key=variable2, value=estimate)  %>%
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")

  