# PROSPER DATA ANALYSIS

#import BRB.tiff data

library(raster)
library(rgdal)

setwd("~/Documents/GitRepos/UrbanStreamflow")

files<- list.files(path = '~/Documents/GitRepos/UrbanStreamflow/BRB/', all.files=TRUE, full.name= TRUE)
rst<-raster(file[1])