library(sna)
library(network)
mat<-network(matrix(0,nc=3,nr=3),directed=FALSE)
mat[1,2]<-1

par(mar=c(0, 0, 0, 0))
plot(mat,interactive=TRUE,vertex.col=c("red","red","blue"),vertex.cex=3)






mat<-network(matrix(0,nc=4,nr=4),directed=FALSE)
mat[1,2]<-1
mat[1,3]<-1
mat[1,4]<-1

mat2<-network(matrix(0,nc=4,nr=4),directed=FALSE)

par(mfrow=c(1,2),mar=c(0, 0, 0, 0))
coord<-plot(mat,vertex.cex=3,label=c("Bill","Bob","Joe","Garry"),label.cex=2)
box()
plot(mat2,vertex.cex=3,label=c("Jeff","Bob","Joe","Garry"),coord=coord,label.cex=2)
box()

library(ergm)

mat<-network(matrix(0,nc=10,nr=10),directed=FALSE)
g.sim <- simulate(mat ~ edges, coef=c(2))

par(mfrow=c(1,2),mar=c(0, 0, 0, 0))
plot(g.sim ,vertex.cex=3)#,label=c("Bill","Bob","Joe","Garry"))
box()
plot(mat2,vertex.cex=3)#,label=c("Bill","Bob","Joe","Garry"))
box()


mat<-network(matrix(0,nc=2,nr=2),directed=FALSE)
mat[1,2]<-1


mat2<-network(matrix(0,nc=4,nr=4),directed=FALSE)

par(mfrow=c(1,2),mar=c(0, 0, 0, 0))
coord=plot(mat,vertex.cex=3,label=c("Bill","Bob"),label.cex=2)
box()
legend("bottomleft",legend="Day 1")

plot(mat,vertex.cex=3,label=c("Bill","Bob"),coord=coord,label.cex=2)
box()
legend("bottomleft",legend="Day 2")





mat<-network(matrix(0,nc=17,nr=17),directed=FALSE)
mat[1,2]<-1
mat[2,3]<-1
mat[3,4]<-1
mat[4,5]<-1
mat[5,6]<-1
mat[6,7]<-1
mat[7,8]<-1
mat[8,9]<-1
mat[9,1]<-1

mat[9,10]<-1
mat[10,11]<-1
mat[11,12]<-1
mat[12,13]<-1
mat[13,14]<-1
mat[14,15]<-1
mat[15,16]<-1
mat[16,17]<-1
mat[17,9]<-1

col<-rep("red",17)
col[9]<-"blue"
par(mar=c(0, 0, 0, 0))
plot(mat,vertex.cex=1.5,vertex.col=col)



### Seasonal example
parl<-day<-cbind(1:7,c(0,1,0,1,0,2,1))
plot.new()
plot.window(ylim=c(0,2),xlim=c(1,7))
rect(5.75,min(parl)-.0009,7.5,max(parl)+.001,col=rgb(1,0,0,.5),border=rgb(1,0,0,.5))
lines(day)
points(day,pch=19,col="blue")
axis(1,at=1:7,labels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
#axis(2)












