#First script to manipulate data
#Economics 691
#Your Name
#Today's Date

rm(list=ls())

library(tidyverse)

covidIL<-read_csv("./Data/ILCovid19.csv", col_names=TRUE, skip_empty_rows = TRUE)

delta<-function(x){
  temp<-((x-lag(x))/lag(x))
  return(round(temp,4))
}

covidIL<-covidIL %>%
  mutate(pc_tests = delta(Tests),
         pc_cases = delta(Cases),
         pc_deaths = delta(Deaths))

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date = as.Date(covidIL$Date, "%m/%d/%Y")

plot(covidIL$Date,covidIL$pc_deaths)

plot(covidIL$Date,covidIL$pc_deaths,
     main="Percent Positive Rates",
     ylab="",
     xlab="Date",
     type="l",
     col="blue")


