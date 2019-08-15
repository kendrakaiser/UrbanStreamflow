#---------------
# Download all data from the Boise Basin for three rain events in February 2019
# Kendra Kaiser January 9th 2018
#---------------
### use fillMiss function in the data retreival package to deal with weird numbers
library(dataRetrieval)
library(tidyverse)

setwd("~/Documents/GitRepos/UrbanStreamflow")

USGS_sites<- read_csv('USGSGages.csv')
siteInfo<-readNWISdata(sites= USGS_sites$Code, service="site")
write_csv(siteInfo, "siteInfo.csv")


start=as.Date("2019-01-16")
end= as.Date("2019-03-06")
pCode <- "00060"

flowdata <- readNWISuv(siteNumbers = siteInfo$site_no, parameterCd = pCode, startDate = start, endDate = end) %>% renameNWISColumns() %>% data.frame

flow<-merge(flowdata, USGS_sites, by.x="site_no", by.y="Code", all=TRUE)

Q<-flow[flow$site_no == "13185000" | flow$site_no == "13206000" | flow$site_no ==  "13211205" | flow$site_no == "13213000",]
Q<-Q[,-6]
write.csv(Q, file="BRB_Q_subset.csv")

Drains<-flow[flow$Cat == 'Drain',]
drainID<-unique(Drains$site_no)

plot(Drains$dateTime[Drains$site_no == drainID[1]], Drains$Flow_Inst[Drains$site_no == drainID[1]], type="l", col="black", ylim=c(0, 60), xlab="Date", ylab="Discharge (cfs)") #eagle 
lines(Drains$dateTime[Drains$site_no == drainID[2]], Drains$Flow_Inst[Drains$site_no == drainID[2]], col= 'blue') #middleton slough
lines(Drains$dateTime[Drains$site_no == drainID[3]], Drains$Flow_Inst[Drains$site_no == drainID[3]], col='green') #S middleton drain
legend("topleft", legend=c("Eagle", "Middleton Slough", "S Middleton"), col=c("black", "blue", "green"), lty=1:2, cex=0.8)
axis.POSIXct(1, at = seq(start, end, by="weeks"), format = "%m-%d")

plot(Drains$dateTime[Drains$site_no == drainID[4]], Drains$Flow_Inst[Drains$site_no == drainID[4]], type="l", col="blue", xlab="Date", ylab="Discharge (cfs)") #Dixie Drain
axis.POSIXct(1, at = seq(start, end, by="weeks"), format = "%m-%d")

Nat<-flow[flow$Cat == 'Natural',]
natID<-unique(Nat$site_no)

plot(Nat$dateTime[Nat$site_no == natID[1]], Nat$Flow_Inst[Nat$site_no == natID[1]], type="l", col="blue", ylim=c(0, 950), xlab="Date", ylab="Discharge (cfs)")
lines(Nat$dateTime[Nat$site_no ==natID[2]], Nat$Flow_Inst[Nat$site_no == natID[2]], col = 'green')
lines(Nat$dateTime[Nat$site_no ==natID[3]], Nat$Flow_Inst[Nat$site_no == natID[3]], col = 'black')
legend("topleft", legend=c("Boise Twin Springs", "SF Boise Featherville", "Mores Creek"), col=c("blue", "green", 'black'), lty=1:2, cex=0.8)
axis.POSIXct(1, at = seq(start, end, by="weeks"), format = "%m-%d")

plot(Nat$dateTime[Nat$site_no == natID[4]], Nat$Flow_Inst[Nat$site_no == natID[4]], type="l", col="red", xlab="Date", ylab="Discharge (cfs)")
axis.POSIXct(1, at = seq(start, end, by="weeks"), format = "%m-%d")
library(XML)
# pu: cumulative water year precip; px: observed daily totatl precip; ID: computed reservoir inflow
url_ark <- "daily_precip_ark.htm" #arrowrock
data_ark <- readHTMLTable(url_ark, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)# Read the HTML table
ark=data_ark[[1]] #convert to data frame

url_and <- "daily_precip_and.htm"# Assign URL - Anderson Ranch
data_and <- readHTMLTable(url_and, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)
and=data_and[[1]]

url_luc <- "luc_daily.htm"#Lucky Peak "luc_daily.htm"
data_luc <- readHTMLTable(url_luc, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)
luc=data_luc[[1]]
luc<-data.frame("Date"= as.Date(luc[,1]), "lucQ"=as.numeric(luc$luc_qd), "lucIn"=as.numeric(luc$luc_id), "luc_unreg"=as.numeric(luc$luc_qu))

luc_spring=luc[7394:7609,]
url_nyc<- "NYC_tenyear.htm"#new york canal
data_nyc<- readHTMLTable(url_nyc, header=TRUE, as.data.frame = TRUE, stringsAsFactors=FALSE)
nyc=data_nyc[[1]]

plot(luc_spring$Date, luc_spring$lucIn, type='l', ylim=c(0,12000))
lines(luc$Date, luc$luc_unreg, col="blue")

nyc_spring<-data.frame("Date"= as.Date(nyc[,1]),"nycQ"=as.numeric(nyc$bsei_qj))

plot(nyc_spring$Date, nyc_spring$nycQ, type='l')

res<-data.frame("Date"= as.Date(and[,1]), "andP"=as.numeric(and$and_pp), "arkP" = as.numeric(ark$ark_pp), "in_computed_ark"=as.numeric(ark$ark_id), "lucQ" = as.numeric(luc$luc_qd), "in_computed_LUC"=as.numeric(luc$luc_id), "in_unreg_LUC"= as.numeric(luc$luc_qu))


Managed<-flow[flow$Cat == 'Managed',]
manID<-unique(Managed$site_no)

plot(Managed$dateTime[Managed$site_no == manID[2]], Managed$Flow_Inst[Managed$site_no == manID[2]], type="l", col="blue", ylim=c(200, 1600), xlab="Date", ylab="Discharge (cfs)")
abline(h=252)
lines(Managed$dateTime[Managed$site_no == manID[3]], Managed$Flow_Inst[Managed$site_no == manID[3]], col='green')
lines(Managed$dateTime[Managed$site_no == manID[6]], Managed$Flow_Inst[Managed$site_no == manID[6]], col='orange')
lines(Managed$dateTime[Managed$site_no == manID[7]], Managed$Flow_Inst[Managed$site_no == manID[7]], col= 'red')
legend("topleft", legend=c("Below Arrowrock", "Glenwood", "Caldwell", "Parma"), col=c("blue", "green", 'orange', 'red'), lty=1:2, cex=0.8)+
axis.POSIXct(1, at = seq(start, end, by="weeks"), format = "%m-%d")



axis(1, Managed$dateTime, format(Managed$dateTime, "%d %d"), cex.axis = .7)

library(lubridate) # work with dates
library(dplyr) 

nyc_spring$doy<-yday(nyc_spring$Date)
nyc_daily<-group_by(nyc_spring, doy)
nycQmean<-summarize(nyc_daily, mean(nycQ, na.rm=TRUE))
nycQsd<-summarize(nyc_daily, sd(nycQ, na.rm=TRUE))
names(nycQmean)<- c("doy", "mean")
names(nycQsd)<- c("doy", "sd")
nycQmean$sdl<- nycQmean$mean - nycQsd$sd
nycQmean$sdu<- nycQmean$mean + nycQsd$sd
nycQmean$mo<- month(nyc_spring$Date[93:458])

plot(luc$Date[93:458], nycQmean$mean, type='l', ylim= c(0, 2500), xlab="Date", ylab="Discharge (cfs)")
lines(luc$Date[93:458], nycQmean$sdl)
lines(luc$Date[93:458], nycQmean$sdu)

luc$lucIn[which(luc$lucIn < 0)]=0

plot(luc$Date, luc$lucIn, type='l', xlab="Date", ylab="Discharge (cfs)")#, ylim=c(0,16000))
lines(luc$Date, luc$luc_unreg, col="blue")

luc$doy<-yday(luc$Date)
luc_daily<-group_by(luc, doy)

lucQ<-summarize(luc_daily, mean(lucIn, na.rm=TRUE))
lucQsd<-summarize(luc_daily, sd(lucIn, na.rm=TRUE))
names(lucQ)<- c("doy", "mean")
names(lucQsd)<- c("doy", "sd")
lucQ$sdl<-lucQ$mean - lucQsd$sd
lucQ$sdu<- lucQ$mean + lucQsd$sd

plot(lucQ$doy, lucQ$mean, type='l', ylim= c(0, 10000), xlab="Date", ylab="Discharge (cfs)")
lines(lucQ$doy,lucQ$sdl)
lines(lucQ$doy, lucQ$sdu)

lucQout<-summarize(luc_daily, mean(lucQ, na.rm=TRUE))
lucQsdout<-summarize(luc_daily, sd(lucQ, na.rm=TRUE))
names(lucQout)<- c("doy", "mean")
names(lucQsdout)<- c("doy", "sd")
lucQout$sdl<-lucQout$mean - lucQsdout$sd
lucQout$sdu<- lucQout$mean + lucQsdout$sd

lucQur<-summarize(luc_daily, mean(luc_unreg, na.rm=TRUE))
lucQsdur<-summarize(luc_daily, sd(luc_unreg, na.rm=TRUE))
names(lucQur)<- c("doy", "mean")
names(lucQsdur)<- c("doy", "sd")
lucQur$sdl<-lucQur$mean - lucQsdur$sd
lucQur$sdu<- lucQur$mean + lucQsdur$sd



plot(lucQ$doy, lucQ$mean, type='l', ylim= c(0, 10000), xlab="Date", ylab="Discharge (cfs)", col='blue')
lines(lucQ$doy,lucQ$sdl, col='blue')
lines(lucQ$doy, lucQ$sdu, col='blue')

plot(luc$Date[93:458], lucQur$mean, type='l', ylim= c(0, 14000), xlab="Date", ylab="Discharge (cfs)", col='blue')
lines(luc$Date[93:458],lucQur$sdl, col='blue')
lines(luc$Date[93:458], lucQur$sdu, col='blue')

lines(luc$Date[93:458], lucQout$mean)
lines(luc$Date[93:458],lucQout$sdl)
lines(luc$Date[93:458], lucQout$sdu)
