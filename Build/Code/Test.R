library(tidyverse)


covidIL <- read_csv("C:/Users/HP/Downloads/ILCovid19.csv")



delta<-function(x){
  temp<-((x-lag(x))/lag(x))
  return(round(temp,4))
}

covidIL<-covidIL %>%
  mutate(pc_tests = delta(Tests),
         pc_cases = delta(Positives),
         pc_deaths = delta(Deaths),
         Date = as.Date(Date,"%m/%d/%Y"))

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

plot(covidIL$Date,covidIL$pc_deaths)
