# Do bi-variate wavelet analysis

setwd("~/Documents/GitRepos/UrbanStreamflow")

library(tidyverse)
library(WaveletComp)
Q=read.csv("BRB_Q_subset.csv")
TwnSprings<-Q[ Q$site_no ==  "13185000",]
Glenwood<-Q[Q$site_no ==  "13206000",]
Caldwell<-Q[Q$site_no ==  "13211205",]
Parma<-Q[Q$site_no ==  "13213000",]

Q1<-list(TwnSprings, Glenwood, Caldwell, Parma) %>% reduce(left_join, by ='dateTime')

my.wc <- analyze.coherency(Q1, my.pair = c("Flow_Inst.x","Flow_Inst.y"),
                           loess.span = 0,
                           dt = 1/24, dj = 1/100,
                           lowerPeriod = 1/2,
                           make.pval = TRUE, n.sim = 10)

wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (days)")