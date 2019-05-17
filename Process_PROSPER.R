# PROSPER DATA ANALYSIS

#import BRB.tiff data

library(raster)
library(rgdal)

setwd("~/Documents/GitRepos/UrbanStreamflow")
ws <- readOGR('~/Documents/GitRepos/UrbanStreamflow/BRB/WatershedBoundary.shp')
crs_shp<-crs(ws)

files<- list.files(path = '~/Documents/GitRepos/UrbanStreamflow/BRB', all.files=TRUE, full.name= TRUE, pattern = "\\.tif$")
rst<-raster(files[1])

rst_p<-projectRaster(rst, crs = crs_shp)


plot(ws[4,])
plot(rst)
plot(ws, add = TRUE)


lowerBoise <- crop(rst, ws[1,])
