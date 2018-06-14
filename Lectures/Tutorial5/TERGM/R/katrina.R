require(networkDynamic)
require(network)
require(ndtv)
require(sna)
require(animation)
require(katrina)
data(katrina.combined)
data(katrina.bydate)

katrina.bydate[[1]]

activate.edges(triangle, e=2:3, at=5)
activate.vertices(x,at = NULL,v = seq_len(network.size(x)))

mapen<-function(net,e){apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})}

buildNetDyn<-function(nl,base){
t<-base

for(i in 1:length(nl)){
net<-as.matrix(nl[[i]],matrix.type="edgelist")
m1<-match(attr(net,"vnames")[net[,1]],base%v%"vertex.names")
m2<-match(attr(net,"vnames")[net[,2]],base%v%"vertex.names")
pl<-cbind(m1,m2)
map<-mapen(t,pl)

for(k in 1:length(map)){
for(j in 1:length(map[[k]])){
activate.edges(t,at=i,e=map[[k]][j])
#
}
}
activate.vertices(t,onset=i,terminus=i,v=which((base%v%"vertex.names")%in%(nl[[i]]%v%"vertex.names")))
print(i)
}

class(t) <- c("networkDynamic", class(t))
t
}
test<-buildNetDyn(katrina.bydate,katrina.combined)
library(RColorBrewer)
col<-c(brewer.pal(12,"Paired"),"black")
b<-sapply(1:length(test%v%"first.appearance"),function(x){col[(test%v%"first.appearance")[x]]})

test%v%"first.appearance2"<-b


slice.par<-list(start=0,end=13,interval=1, aggregate.dur=1,rule="any")
windsurfers <-compute.animation(test,slice.par=slice.par,animation.mode="MDSJ",default.dist=3)
render.animation(windsurfers,render.par=list(tween.frames=25,show.time=T),vertex.col="fema.region",edge.col="darkgray",displaylabels=F,label.cex=.6,label.col="blue")



render.animation(windsurfers,render.par=list(tween.frames=25,show.time=F),vertex.col="first.appearance2",edge.col="darkgray",displaylabels=F,label.cex=.6,label.col="blue")
saveVideo(ani.replay(),video.name="windsurfers_dailyDraft.mp4", other.opts="-b 1000k")




load("/Users/zack/Desktop/A____US_CENSUS2010/UScensus2010/data/continentalUS.rda")

m<-match(katrina.combined%v%"hq.state",continentalUS$acronym)

col<-unique(m)
col<-col[!is.na(col)]
col2<-rep("white",49)
col2[col]<-col

par(mar=c(0, 0, 0, 0) + 0.1)
plot(continentalUS,col=col2)







