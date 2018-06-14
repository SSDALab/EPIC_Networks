library(spatstat)
library(sna)
library(network)

r<-1.2
size<-50
circle<-disc(radius=1.3)
circle2<-disc(radius=1.3,centre=c(3,0))
points1<-rpoint(size,win=disc(radius=r))
points2<-rpoint(size,win=disc(radius=r,centre=c(3,0)))
n<-npoints(points1)
pdf("pop1.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=cbind(points1$x,points1$y),vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=cbind(points2$x,points2$y),vertex.cex=2,vertex.col="blue")
dev.off()


sizeN<-10
points1N<-rpoint(sizeN,win=disc(radius=r))
points2N<-rpoint(sizeN,win=disc(radius=r,centre=c(3,0)))
nN<-npoints(points1N)

pdf("pop2.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=cbind(points1$x,points1$y),vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=cbind(points2$x,points2$y),vertex.cex=2,vertex.col="blue")
plot(network(matrix(0,nc=nN,nr=nN)),new=FALSE,jitter=FALSE,coord=cbind(points1N$x,points1N$y),vertex.cex=3,vertex.col="white")
plot(network(matrix(0,nc=nN,nr=nN)),new=FALSE,jitter=FALSE,coord=cbind(points2N$x,points2N$y),vertex.cex=3,vertex.col="white")
dev.off()



pdf("pop3.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=n+nN,nr=n+nN)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points1$x,points1$y),cbind(points1N$x,points1N$y)),vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=n+nN,nr=n+nN)),new=FALSE,jitter=FALSE,coord=rbind(cbind(points2$x,points2$y),cbind(points2N$x,points2N$y)),vertex.cex=2,vertex.col="blue")
dev.off()


sizeD<-15
r1<-sample(1:(n+nN),sizeD)
r2<-sample(1:(n+nN),sizeD)
p1<-rbind(cbind(points1$x,points1$y),cbind(points1N$x,points1N$y))
p2<-rbind(cbind(points2$x,points2$y),cbind(points2N$x,points2N$y))
n<-n+nN

pdf("pop4.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=p2,vertex.cex=2,vertex.col="blue")
points(p1[r1,],pch=4,col="white",cex=2,lwd=4)
points(p2[r2,],pch=4,col="white",cex=2,lwd=4)
dev.off()

n<-n-length(r1)
p1<-p1[-r1,]
p2<-p2[-r2,]

pdf("pop5.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=p2,vertex.cex=2,vertex.col="blue")
dev.off()

pdf("pop6.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=n,nr=n)),new=FALSE,jitter=FALSE,coord=p2,vertex.cex=2,vertex.col="blue")
dev.off()

migS<-15
rm1<-sample(1:n,migS)
points2N2<-rpoint(migS,win=disc(radius=r,centre=c(3,0)))
p2n<-rbind(p2,cbind(points2N2$x,points2N2$y))
col1<-rep("red",dim(p1)[1])
col1[rm1]<-rgb(1,0,0,.5)
col2<-rep("blue",NROW(p2n))
col2[(NROW(p2)+1):NROW(p2n)]<-"red"

pdf("pop7.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=dim(p1)[1],nr=dim(p1)[1]))
,new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col=col1)
plot(network(matrix(0,nc=NROW(p2n),nr=nrow(p2n))),new=FALSE,jitter=FALSE,coord=p2n,vertex.cex=2,vertex.col=col2)
ar1<-p1[rm1,]
ar2<-cbind(points2N2$x,points2N2$y)
arrows(x0=ar1[,1],y0=ar1[,2],x1=ar2[,1],y1=ar2[,2],lwd=2,angle=30)
dev.off()

p1<-p1[-rm1,]
pdf("pop8.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=dim(p1)[1],nr=dim(p1)[1]))
,new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=NROW(p2n),nr=nrow(p2n))),new=FALSE,jitter=FALSE,coord=p2n,vertex.cex=2,vertex.col="blue")
dev.off()


pdf("net1.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(network(matrix(0,nc=dim(p1)[1],nr=dim(p1)[1]))
,new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col="red")
plot(network(matrix(0,nc=NROW(p2n),nr=nrow(p2n))),new=FALSE,jitter=FALSE,coord=p2n,vertex.cex=2,vertex.col="blue")
dev.off()



pdf("net2.pdf",width=5,height=4)
library(ergm)
net1<-network(matrix(0,nc=dim(p1)[1],nr=dim(p1)[1]),directed=FALSE)
net2<-network(matrix(0,nc=NROW(p2n),nr=nrow(p2n)),directed=FALSE)
net1%v%"gender"<-rbinom(network.size(net1),1,.5)
net2%v%"gender"<-rbinom(network.size(net2),1,.5)
net1<-simulate(net1 ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))

net2<-simulate(net2 ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))

par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(net1,new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col=rgb(1,0,0,.8))
plot(net2,new=FALSE,jitter=FALSE,coord=p2n,vertex.cex=2,vertex.col=rgb(0,0,1,.8))
dev.off()


d1<-order(degree(net1),decreasing=TRUE)
d2<-order(degree(net2),decreasing=TRUE)
net1[d1[1:20],]<-0
net2[d2[1:20],]<-0

pdf("net3.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(net1,new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col=rgb(1,0,0,.7),lwd=1.5)
plot(net2,new=FALSE,jitter=FALSE,coord=p2n,vertex.cex=2,vertex.col=rgb(0,0,1,.7),lwd=1.5)
dev.off()



pdf("net4.pdf",width=5,height=4)
net1[,]<-0
net2[,]<-0
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(net1,new=FALSE,jitter=FALSE,coord=p1,vertex.cex=2,vertex.col=rgb(1,0,0,.7),lwd=1.5)
plot(net2,new=FALSE,jitter=FALSE,coord=p2n,vertex.cex=2,vertex.col=rgb(0,0,1,.7),lwd=1.5)
dev.off()


net1[1,2]<-1
net1[2,3]<-1
net1[1,3]<-1

net2[1,2]<-1
net2[2,3]<-1
net2[1,3]<-1
net1<-delete.vertices(net1,v=4:network.size(net1))
net2<-delete.vertices(net2,v=4:network.size(net2))

pdf("net5.pdf",width=5,height=4)
par(mar=c(0, 0, 0, 0) + 0.1)
plot.new()
plot.window(xlim=c(-2,4.5),ylim=c(-2.5,2.5))
plot(circle,main="",col="lightgray",add=TRUE)
plot(circle2,main="",col="lightgray",add=TRUE)
plot(net1,new=FALSE,jitter=FALSE,coord=p1[1:3,],vertex.cex=4,vertex.col=rgb(1,0,0,.9),lwd=3)
plot(net2,new=FALSE,jitter=FALSE,coord=p2n[1:3,],vertex.cex=4,vertex.col=rgb(0,0,1,.9),lwd=3)
dev.off()






