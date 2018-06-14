library(spatstat)
library(sna)
library(network)

pdf("pop1.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
set.seed(1925)
points1<-rpoint(100,win=disc())
set.seed(1900)
points2<-rpoint(100,win=disc())
circle<-disc(radius=1.3)
n<-npoints(points1)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=cbind(points1$x,points1$y),vertex.cex=1.5)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=cbind(points2$x,points2$y),vertex.cex=1.5)
dev.off()


pdf("pop2.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
set.seed(1925)
points1<-rpoint(100,win=disc())
set.seed(2010)
points1.1<-rpoint(20,win=disc())

set.seed(1900)
points2<-rpoint(100,win=disc())
set.seed(1776)
points2.2<-rpoint(20,win=disc())
circle<-disc(radius=1.3)
n<-npoints(points1)
n2<-npoints(points1.1)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2,nr=n+n2)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points1$x,points1$y),cbind(points1.1$x,points1.1$y)),vertex.col=c(rep("red",100),rep("blue",20)),vertex.cex=1.5)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2,nr=n+n2)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points2$x,points2$y),cbind(points2.2$x,points2.2$y)),vertex.col=c(rep("red",100),rep("blue",20)),vertex.cex=1.5)
dev.off()




pdf("pop3.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
plot(circle,main="",col="lightgray")
plot(network(matrix(0,nc=n+n2,nr=n+n2)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points1$x,points1$y),cbind(points1.1$x,points1.1$y)),vertex.col=c(rep("red",100),rep("red",20)),vertex.cex=1.5)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2,nr=n+n2)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points2$x,points2$y),cbind(points2.2$x,points2.2$y)),vertex.col=c(rep("red",100),rep("red",20)),vertex.cex=1.5)
dev.off()


set.seed(1922)
r<-sample(1:length(points1$x),10)

pdf("pop4.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
plot(circle,main="",col="lightgray")
plot(network(matrix(0,nc=n+n2,nr=n+n2)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points1$x,points1$y),cbind(points1.1$x,points1.1$y)),vertex.col=c(rep("red",100),rep("red",20)),vertex.cex=1.5)
points(points1$x[r],points1$y[r],pch=4,col="white",cex=1.5,lwd=4)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2,nr=n+n2)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points2$x,points2$y),cbind(points2.2$x,points2.2$y)),vertex.col=c(rep("red",100),rep("red",20)),vertex.cex=1.5)
points(points2$x[r],points2$y[r],pch=4,col="white",cex=1.3,lwd=4)
dev.off()

pdf("pop5.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
plot(circle,main="",col="lightgray")
plot(network(matrix(0,nc=n+n2-length(r),nr=n+n2-length(r))),new=FALSE,jitter=FALSE,coord=rbind(cbind(points1$x[-r],points1$y[-r]),cbind(points1.1$x,points1.1$y)),vertex.col=c(rep("red",100),rep("red",10)),vertex.cex=1.5)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2-length(r),nr=n+n2-length(r))),new=FALSE,jitter=FALSE,coord=rbind(cbind(points2$x[-r],points2$y[-r]),cbind(points2.2$x,points2.2$y)),vertex.col=c(rep("red",100),rep("red",10)),vertex.cex=1.5)
dev.off()

pdf("migration1.pdf",width=6,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
plot(circle,main="",col="lightgray")
plot(network(matrix(0,nc=n+n2-length(r),nr=n+n2-length(r))),new=FALSE,jitter=FALSE,coord=rbind(cbind(points1$x[-r],points1$y[-r]),cbind(points1.1$x,points1.1$y)),vertex.col=c(rep("red",100),rep("red",10)),vertex.cex=1.5)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2-length(r),nr=n+n2-length(r))),new=FALSE,jitter=FALSE,coord=rbind(cbind(points2$x[-r],points2$y[-r]),cbind(points2.2$x,points2.2$y)),vertex.col=c(rep("blue",100),rep("r",10)),vertex.cex=1.5)
dev.off()




#pdf("migration2.pdf",width=4,height=4)
library(RSvgDevice)
devSVG(file="migration2.svg",width=4,height=6,onefile=TRUE)
set.seed(1955)
r1<-sample(1:length(points1$x),30)
set.seed(1870)
r2<-sample(1:length(points1$x),30)

c1<-rbind(cbind(points1$x[-r],points1$y[-r]),cbind(points1.1$x,points1.1$y))
c2<-rbind(cbind(points2$x[-r],points2$y[-r]),cbind(points2.2$x,points2.2$y))
col1<-c(rep("red",100),rep("red",10))
col2<-c(rep("blue",100),rep("blue",10))
col1.t<-col1
col1.t[r1]<-col2[r1]
col2.t<-col2
col2.t[r2]<-col1[r2]


par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
plot(circle,main="",col="lightgray")
plot(network(matrix(0,nc=n+n2-length(r),nr=n+n2-length(r))),new=FALSE,jitter=FALSE,coord=c1,vertex.col=col1.t,vertex.cex=1.5)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(network(matrix(0,nc=n+n2-length(r),nr=n+n2-length(r))),new=FALSE,jitter=FALSE,coord=c2,vertex.col=col2.t,vertex.cex=1.5)
dev.off()


pdf("migration2.pdf",width=6,height=4)
par(mfrow=c(1,1),mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-1,3.5),ylim=c(-1.5,1.5))
plot(circle,main="",add=TRUE,col="lightgray")
plot(circle2,main="",add=TRUE,col="lightgray")

set.seed(1955)
r1<-sample(1:length(points$x),10)
set.seed(1870)
r2<-sample(1:length(points$x),20)

points(points$x,points$y,pch=19,col="red",cex=.8)
points(points2$x,points2$y,pch=19,col="blue",cex=.8)
points(points$x[r1],points$y[r1],pch=19,col="blue",cex=1)
points(points2$x[r2],points2$y[r2],pch=19,col="red",cex=1)
dev.off()


##################
### relational Analysis
##################
library(networkSpatial)
set.seed(1984)
points<-rpoint(100,win=disc())
n<-npoints(points)
net<-network(matrix(0,nc=n,nr=n),directed=FALSE)

pdf("net.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
circle<-disc(radius=1.3)
plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)

plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)

dev.off()

pdf("net_1.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
circle<-disc(radius=1.3)
net[1,1:network.size(net)]<-1
plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)

plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)

dev.off()



pdf("net2.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
circle<-disc(radius=1.3)
net[1,1:network.size(net)]<-1
plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)

plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)
dev.off()

pdf("net2_2.pdf",width=4,height=4)
par(mfrow=c(1,2),mar=c(0, 0, 0, 0) + 0.1)
circle<-disc(radius=1.3)
net[1,1:network.size(net)]<-0
plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)

plot(circle,main="",col="lightgray")
plot(net,coord=cbind(points$x,points$y),new=FALSE,vertex.cex=1.5)
dev.off()




library(diffusion)
set.edge.attribute(net,"diffrate",1)  #Set all edges to lambda=1
net%v%"diffstate"<-c(1,rep(0,9))      #Create one seed vertex
net<-diffusion(net)
dtrace<-diffusion.forest(net)
depth<-dtrace%v%"diffdepth"
depth[is.na(depth)]<-0

pdf("net3.pdf",width=4,height=4)
par(mfrow=c(1,1),mar=c(0, 0, 0, 0) + 0.1)

circle<-disc(radius=1.1)
plot(circle,main="",col="lightgray")
#points(points$x,points$y,pch=19,col="red",cex=.6)
plot(dtrace,coord=cbind(points$x,points$y),new=FALSE,vertex.col=depth)
dev.off()














