library(UScensus2000)
library(UScensus2000blk)
library(gpclib)
gpclibPermit()
#source("intersectCityLevel.R")
data(massachusetts.blk)


 sf.c<-city("boston","ma")
 
 index<-overlay(SpatialPoints(coordinates(massachusetts.blk)),sf.c)
 sf2<-massachusetts.blk[which(!is.na(index)),]
 save(sf2,file="sf2.rda")
##############
## checks
###############

png("sf.png")
plot(sf2)
dev.off()

