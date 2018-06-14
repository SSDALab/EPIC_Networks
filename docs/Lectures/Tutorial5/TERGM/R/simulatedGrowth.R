library(spatstat)
library(networkSpatial)
library(sna)
library(network)

#pdf("pop1.pdf",width=4,height=4)
#par(mfrow=c(1,1),mar=c(0, 0, 0, 0) + 0.1)
#set.seed(1984)
#points<-rpoint(100,win=disc())
#circle<-disc(radius=1.1)
#plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
#dev.off()

#(0.533, .032, 2.788)
#set.seed(1984)
#points<-rpoint(100,win=disc())
net6<-vector("list",9)
points<-vector("list",9)
for(i in 1:9){
set.seed(1984)
points[[i]]<-rpoint(exp(.3*i)*10,win=disc())
net6[[i]]<-rnspatial(1,cbind(points[[i]]$x,points[[i]]$y),c(0.533, 6,2.788),model="atpowlaw")
}


par(mfrow=c(3,3),mar=c(0, 0, 0, 0)+.1)
circle<-disc(radius=1.1)
for(i in 1:9){
plot(circle,main="",col="lightgray")
if(i==1){
col<-rep(rgb(0,0,1,.5),length(points[[i]]$x))	
}else{
col<-c(rep(rgb(0,0,1,.5),length(points[[i-1]]$x)),rep(rgb(1,0,0,.5),(length(points[[i]]$x)-length(points[[i-1]]$x))))
	}
plot(net6[[i]],coord=cbind(points[[i]]$x,points[[i]]$y),new=FALSE,vertex.cex=1.5,edge.col=rgb(0,0,0,.5),vertex.col=col)
}