#---------------
# Download all data from the Boise Basin for three rain events in February 2019
# Kendra Kaiser January 9th 2018
#---------------

library(dataRetrieval)
library(tidyverse)

setwd("~/Documents/GitRepos/UrbanStreamflow")

USGS_sites<- read_csv('USGSGages.csv')
siteInfo<-readNWISdata(sites= USGS_sites$Code, service="site")
write_csv(siteInfo, "siteInfo.csv")


start=as.Date("2019-01-16")
end= as.Date("2019-03-16")
pCode <- "00060"


flowdata <- readNWISuv(siteNumbers = siteInfo$site_no, parameterCd = pCode, startDate = start, endDate = end) %>% renameNWISColumns() %>% data.frame

flow<-merge(flowdata, USGS_sites, by.x="site_no", by.y="Code", all=TRUE)

Drains<-flow[flow$Cat == 'Drain',]
drainID<-unique(Drains$site_no)

plot(Drains$dateTime[Drains$site_no == drainID[1]], Drains$Flow_Inst[Drains$site_no == drainID[1]], type="l", col="blue", ylim=c(0, 80))
for (i in 2:3){
  lines(Drains$dateTime[Drains$site_no == drainID[i]], Drains$Flow_Inst[Drains$site_no == drainID[i]])
}
plot(Drains$dateTime[Drains$site_no == drainID[4]], Drains$Flow_Inst[Drains$site_no == drainID[4]], type="l", col="blue") #Dixie Drain

Nat<-flow[flow$Cat == 'Natural',]
natID<-unique(Nat$site_no)

plot(Nat$dateTime[Nat$site_no == natID[1]], Nat$Flow_Inst[Nat$site_no == natID[1]], type="l", col="blue", ylim=c(0, 950))
lines(Nat$dateTime[Nat$site_no ==natID[2]], Nat$Flow_Inst[Nat$site_no == natID[2]])
lines(Nat$dateTime[Nat$site_no ==natID[3]], Nat$Flow_Inst[Nat$site_no == natID[3]], col = 'green')

plot(Nat$dateTime[Nat$site_no == natID[4]], Nat$Flow_Inst[Nat$site_no == natID[4]], type="l", col="blue")
