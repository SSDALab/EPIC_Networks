library(UScensus2000)
library(UScensus2000blk)
library(gpclib)
gpclibPermit()
#source("intersectCityLevel.R")
data(new_york.blk)


 sf.c<-city("Stony Brook","NY")
 
 index<-overlay(SpatialPoints(coordinates(new_york.blk)),sf.c)
 sf2<-new_york.blk[which(!is.na(index)),]
 save(sf2,file="sf2.rda")
##############
## checks
###############

png("sf.png")
plot(sf2)
dev.off()

