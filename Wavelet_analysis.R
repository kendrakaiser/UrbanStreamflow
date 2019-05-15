# Do bi-variate wavelet analysis

setwd("~/Documents/GitRepos/UrbanStreamflow")

library(tidyverse)
library(WaveletComp)
Q=read.csv("BRB_Q_subset.csv")
TwnSprings<-Q[ Q$site_no ==  "13185000",]
Glenwood<-Q[Q$site_no ==  "13206000",]
Caldwell<-Q[Q$site_no ==  "13211205",]
Parma<-Q[Q$site_no ==  "13213000",]
Featherville<-Q[Q$site_no ==  "13186000",]

Q1<-list(TwnSprings, Glenwood, Caldwell, Parma) %>% reduce(left_join, by ='dateTime')

my.wc <- analyze.coherency(Q1, my.pair = c("Flow_Inst.x","Flow_Inst.y"),
                           loess.span = 0,
                           dt = 1/4, dj = 1/100,
                           lowerPeriod = 1/2,
                           make.pval = TRUE, n.sim = 10)





par(mfrow=c(3,1))

plot(Q1$dateTime, Q1$Flow_Inst.x, type='l', col = 'black')
lines(Q1$dateTime, Q1$Flow_Inst.y, col= 'blue')

wc.image(my.wc, n.levels = 250,
         legend.params = list(lab = "cross-wavelet power levels"),
         timelab = "", periodlab = "period (hrs)")

wc.avg(my.wc, siglvl = 0.01, sigcol = "red", sigpch = 20,
       periodlab = "period (days)")


#alternative option - decompose time serreis using STL
fit<- stl(Q$Flow_Inst.x)