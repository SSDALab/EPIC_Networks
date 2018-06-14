#test drawing networks with weighted edges
enable=F
if (enable){

library(network)
library(networkDynamic)
library(ndtv)
test <- network.initialize(5,directed=F)
test[1,2]<-1
test[3,2]<-1
test[1,3]<-1
test[1,4]<-1
test[5,1]<-1
test[4,5]<-1
test <-ndtv:::activate.edge.attribute(test,'weight',value=1,onset=0,terminus=1,e=c(1,2,3))
test <- ndtv:::activate.edge.attribute(test,'weight',value=3,onset=0,terminus=1,e=c(4,5,6))
test <- ndtv:::activate.edge.attribute(test,'weight',value=3,onset=1,terminus=2,e=c(1,2,3))
test <- ndtv:::activate.edge.attribute(test,'weight',value=1,onset=1,terminus=2,e=c(4,5,6))
slice.par<-list(start=0,end=2,interval=1, aggregate.dur=1,rule="any")
test <-compute.animation(test,slice.par=slice.par,animation.mode="kamadakawai")
render.animation(test,render.par=list(tween.frames=25,show.time=T),edge.col="#55555555",displaylabels=T,label.cex=.6,label.col="blue",edge.lwd='active')

#test a network with edge weights as distances

#test a network with edge weights as similarities

#test a network with edge weights as a variable

#test symetrizing an asymetric network


}
