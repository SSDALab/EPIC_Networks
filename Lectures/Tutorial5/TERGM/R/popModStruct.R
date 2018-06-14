net1<-network(matrix(0,nc=3,nr=3),directed=FALSE)
net2<-network(matrix(0,nc=3,nr=3),directed=FALSE)




circle<-disc(radius=1.3)
circle2<-disc(radius=1.3,centre=c(3,0))

p1<-rpoint(3,win=disc(radius=r))
p2<-rpoint(3,win=disc(radius=r,centre=c(3,0)))


net1[1,2]<-1
net1[2,3]<-1
net1[1,3]<-0

net2[1,2]<-1
net2[2,3]<-0
net2[1,3]<-1


pdf("net5.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(net1,new=FALSE,jitter=FALSE,coord=cbind(p1$x,p1$y),vertex.cex=4,vertex.col=rgb(1,0,0,.9),lwd=3)#,displaylabels=TRUE)
plot(net2,new=FALSE,jitter=FALSE,coord=cbind(p2$x,p2$y),vertex.cex=4,vertex.col=rgb(0,0,1,.9),lwd=3)#,displaylabels=TRUE)
dev.off()


net1[1,2]<-1
net1[2,3]<-1
net1[1,3]<-1

net2[1,2]<-1
net2[2,3]<-1
net2[1,3]<-1


pdf("net6.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(net1,new=FALSE,jitter=FALSE,coord=cbind(p1$x,p1$y),vertex.cex=4,vertex.col=rgb(1,0,0,.9),lwd=3)#,displaylabels=TRUE)
plot(net2,new=FALSE,jitter=FALSE,coord=cbind(p2$x,p2$y),vertex.cex=4,vertex.col=rgb(0,0,1,.9),lwd=3)#,displaylabels=TRUE)
dev.off()