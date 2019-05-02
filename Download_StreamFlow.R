#---------------
# Download all data from the Boise Basin for three rain events in February 2019
# Kendra Kaiser January 9th 2018
#---------------

library(dataRetrieval)
library(tidyverse)

setwd("~/Documents/GitRepos/UrbanStreamflow")

USGS_sites<- read_csv('USGSGages.csv')
siteInfo<-readNWISdata(sites= USGS_sites$Code, service="site")

library(maps)
library(mapdata)
par(las = 1, tck = 0.02, mar = c(0, 0, 0, 0))
map("state", region = c("Idaho"))
map("rivers", add = TRUE, col = 4)
# label centered over gage site, jitter added to differentiate sites close together
mindif <- 0
maxiterations <- 30
iteration <- 1
while (mindif < 0.085) {
  y.offset <- as.numeric(my.siteInfo$lat) + runif(length(my.siteInfo$lat),0.12, 0.45)
  mindif <- min(diff(unique(sort.int(round(as.numeric(y.offset), digits = 3)))))
  iteration <- iteration + 1
  if ( iteration >= maxiterations ) {
    mindif <- 0.09
    message("No ideal jitter found. Some labels may conflict")
     }
  }
points(my.siteInfo$lng, my.siteInfo$lat, pch = 19, col = "green")
text(xy.coords(my.siteInfo$lng, y.offset), my.siteInfo$staid, cex = 0.55)
box()
map.axes()


start=as.Date("2019-01-16")
end= as.Date("2019-03-16")