
pdf("bgtitle.pdf")
par(mar=c(0, 0, 0, 0))
plot.new()
plot.window(ylim=c(0,5),xlim=c(0,5))
rect(0,0,5,5,col=rgb(0,0,1,.8),border="transparent")
dev.off()




library(katrina)
data(katrina.bydate)
data(katrina.combined)
colkat<-function(net,net2){
require(RColorBrewer)
col<-c(brewer.pal(12,"Paired"),"black")
b<-sapply(1:length(net%v%"first.appearance"),function(x){col[(net%v%"first.appearance")[x]]})
net%v%"first.appearance2"<-b
net



for(i in 1:length(net2)){
	m<-match(net2[[i]]%v%"vertex.names",net%v%"vertex.names")
	net2[[i]]%v%"first.appearance2"<-(net%v%"first.appearance2")[m]
}
net2
}


colkat<-function(net,net2){
require(RColorBrewer)
col<-c(brewer.pal(9,"PuBuGn"),"black","gray")
fr<-net%v%"fema.region"
fr[is.na(fr)]<-11
net%v%"fema.region"<-fr
b<-sapply(1:length(net%v%"fema.region"),function(x){col[(net%v%"fema.region")[x]]})
net%v%"first.appearance2"<-b
net



for(i in 1:length(net2)){
	m<-match(net2[[i]]%v%"vertex.names",net%v%"vertex.names")
	net2[[i]]%v%"first.appearance2"<-(net%v%"first.appearance2")[m]
}
net2
}


kbd<-colkat(katrina.combined,katrina.bydate)
par(mfrow=c(1,13),mar=c(0, 0, 0, 0))
for(i in 1:13){
	plot(kbd[[i]],vertex.cex=.95,vertex.col="first.appearance2",vertex.border="first.appearance2")
}