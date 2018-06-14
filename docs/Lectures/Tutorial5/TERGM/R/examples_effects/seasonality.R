
par<-c(-3.4522,-3.3754,-3.0218,-2.8769,-3.0973,-1.9258,-2.2235)
logit<-function(x){
	exp(x)/(1+exp(x))
}



parl<-sapply(par,logit)


plot.new()
plot.window(ylim=c(min(parl),max(parl)),xlim=c(1,length(parl)))
rect(5.75,min(parl)-.0009,7.5,max(parl)+.001,col=rgb(1,0,0,.5),border=rgb(1,0,0,.5))
lines(parl)
points(parl,pch=19,col="blue")
axis(1,at=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
axis(2)
title(ylab="Baseline Probability")

########
## Edge
#######
library(network)
library(sna)
par<-c(-12.2986,-9.5061,-10.6229,-10.6006,-11.6135,-11.4279,-12.5474)
conPar<-function(x){log(x)*4.0946}
load("/Users/zack/Desktop/WindSurfers/windsurfers.rda")
size<-sapply(beach[!is.na(beach)],network.size)
dayWeek<-sapply(beach[!is.na(beach)],function(x){x%n%"day"})
OneWeek<-size[5:11]
conParOW<-sapply(OneWeek,conPar)

logit<-function(x){
	exp(sum(x))/(1+exp(sum(x)))
}

out<-cbind(par,conParOW)

edge<-apply(out,1,logit)

parl<-edge
plot.new()
plot.window(ylim=c(min(parl),max(parl)),xlim=c(1,length(parl)))
rect(5.75,min(parl)-.0009,7.5,max(parl)+.01,col=rgb(1,0,0,.5),border=rgb(1,0,0,.5))
lines(parl)
points(parl,pch=19,col="blue")
axis(1,at=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
axis(2)
title(ylab="Baseline Density")

