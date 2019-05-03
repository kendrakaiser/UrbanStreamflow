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
lines(Nat$dateTime[Nat$site_no ==natID[2]], Nat$Flow_Inst[Nat$site_no == natID[2]], col = 'green')
lines(Nat$dateTime[Nat$site_no ==natID[3]], Nat$Flow_Inst[Nat$site_no == natID[3]], col = 'black')
legend("topleft", legend=c("Boise Twin Springs", "SF Boise Featherville", "Mores Creek"), col=c("blue", "green", 'black'), lty=1:2, cex=0.8)
plot(Nat$dateTime[Nat$site_no == natID[4]], Nat$Flow_Inst[Nat$site_no == natID[4]], type="l", col="blue")

library(XML)
# pu: cumulative water year precip; px: observed daily totatl precip; ID: computed reservoir inflow
url_ark <- "daily_precip_ark.htm" #arrowrock
data_ark <- readHTMLTable(url_ark, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)# Read the HTML table
ark=data_ark[[1]] #convert to data frame

url_and <- "daily_precip_and.htm"# Assign URL - Anderson Ranch
data_and <- readHTMLTable(url_and, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)
and=data_and[[1]]

url_luc <- "daily_luc_spring.htm"#Lucky Peak "luc_daily.htm"
data_luc <- readHTMLTable(url_luc, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)
luc=data_luc[[1]]

url_nyc<- "nyc_daily.htm"#new york canal
data_nyc<- readHTMLTable(url_nyc, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)
nyc=data_nyc[[1]]

luc_spring<-data.frame("Date"= as.Date(luc[,1]), "lucQ"=as.numeric(luc$luc_qd), "lucI"=as.numeric(luc$luc_id))
plot(luc_spring$Date, luc_spring$lucQ, type='l')
nyc_spring<-data.frame("Date"= as.Date(nyc[,1]),"nycQ"=as.numeric(nyc$bsei_qj))
plot(nyc_spring$Date, nyc_spring$nycQ, type='l')
res<-data.frame("Date"= as.Date(and[,1]), "andP"=as.numeric(and$and_pp), "arkP" = as.numeric(ark$ark_pp), "in_computed_ark"=as.numeric(ark$ark_id), "lucQ" = as.numeric(luc$luc_qd), "in_computed_LUC"=as.numeric(luc$luc_id), "in_unreg_LUC"= as.numeric(luc$luc_qu))


Managed<-flow[flow$Cat == 'Managed',]
manID<-unique(Managed$site_no)
plot(res$Date, res$lucQ, col='black', type='l')
plot(Managed$dateTime[Managed$site_no == manID[2]], Managed$Flow_Inst[Managed$site_no == manID[2]], type="l", col="blue", ylim=c(200, 3000))
lines(Managed$dateTime[Managed$site_no == manID[3]], Managed$Flow_Inst[Managed$site_no == manID[3]], col='green')
lines(Managed$dateTime[Managed$site_no == manID[6]], Managed$Flow_Inst[Managed$site_no == manID[6]], col='orange')
lines(Managed$dateTime[Managed$site_no == manID[7]], Managed$Flow_Inst[Managed$site_no == manID[7]], col= 'red')
legend("topleft", legend=c("Below Arrowrock", "Glenwood", "Caldwell", "Parma"), col=c("blue", "green", 'orange', 'red'), lty=1:2, cex=0.8)

