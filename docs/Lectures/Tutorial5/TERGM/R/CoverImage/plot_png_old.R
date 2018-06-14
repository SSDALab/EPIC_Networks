library(network)
library(diffusion)
library(sna)
library(gpclib)
library(spatstat)
library(rgdal)
library(multicore)
library(maptools)

load("portlandNetwork.rda")
load("diffusion2.rda")

as.Polygons.gpc.poly <- function(x, ID) {
            thisPolys <- lapply(get.pts(x), function(p) {
                Polygon(rbind(as.matrix(cbind(p$x, p$y)), c(p$x[1], 
                  p$y[1])), hole = p$hole)
            })
            Polygons(thisPolys, ID)
  }

buildBB<-function(city,bb.epsilon){
	gpclibPermit()
  	temp1 <- bbox(city)
        temp1[1, 1] <- temp1[1, 1] - bb.epsilon
        temp1[1, 2] <- temp1[1, 2] + bb.epsilon
        temp1[2, 1] <- temp1[2, 1] - bb.epsilon
        temp1[2, 2] <- temp1[2, 2] + bb.epsilon
        temp <- matrix(c(temp1[1, 1], temp1[2, 1], temp1[1, 2], 
            temp1[2, 1], temp1[1, 2], temp1[2, 2], temp1[1, 1], 
            temp1[2, 2]), nc = 2, nr = 4, byrow = TRUE)
        temp.gcp <- as(temp, "gpc.poly")
        temp.pb <- as.Polygons.gpc.poly(temp.gcp, "temp.gcp1")
        SpatialPolygons(list(temp.pb), proj4string = CRS(proj4string(city)))
  }


gplot.spatial<-function(x,coord,bb,list.edges,list.vertex){
	#require(geosphere)
	plot.new()
	plot.window(xlim=bb[1,],ylim=bb[2,],asp=1)
	axis(1,cex.axis=.5,lwd.ticks=.5,las=1)
	axis(2,cex.axis=.5,lwd.ticks=.5)
	box()
	x<-as.matrix.network(x,"edgelist")
	#for(i in 1:NROW(x)){
	do.call("lines",c(list(x=rbind(coord[x[,1],],coord[x[,2],])),list.edges))      
	#}     
	do.call("points",c(list(x=coord),list.vertex))
	#title(xlab="meters (m)",ylab="meters (m)")          
}

gplot.spatial2<-function(x,coord,bg,bb,list.edges,list.vertex){
	#require(geosphere)
	plot.new()
	plot.window(xlim=bb[1,],ylim=bb[2,],asp=1)
	plot(bg,border="white",col="white",add=TRUE)
	axis(1,cex.axis=.4,lwd=.5,las=1)#tick=FALSE,lwd=.5)
	axis(2,cex.axis=.4,lwd=.5)#tick=FALSE,lwd=.5)
	
	x<-as.matrix.network(x,"edgelist")
	#for(i in 1:NROW(x)){
	do.call("lines",c(list(x=rbind(coord[x[,1],],coord[x[,2],])),list.edges))      
	#}     
	do.call("points",c(list(x=coord),list.vertex)) 
	box(lwd=1.5)         
}


coord<-coordinates(ihp.pts)
depth<-net%v%"diffdepth"
col<-rep("",length(depth))
col[is.na(depth)]<-"black"
col[depth==0]<-"red"
depth[depth==0]<-NA
quant<-quantile(depth,na.rm=TRUE)
colors<-c("orange","yellow","green","blue") #heat.colors(4)
# red,orange,yellow,green,blue,purple
col[depth<=quant[2]]<-colors[1]
col[quant[2]<depth & depth<=quant[3]]<-colors[2]
col[quant[3]<depth & depth<=quant[4]]<-colors[3]
col[ quant[4]<depth & depth<=quant[5]]<-colors[4]
table(col)

###### Quick function for performing geographic cut
#depth<-net%v%"diffdepth"
depth<-net%v%"diffdepth"
points<-ihp.pts[which(depth==0),]
index<-overlay(points,portland.proj)
index<-unique(index)
index<-index[!is.na(index)]
poly<-buildBB(portland.proj[index,],.05)
index.net<-overlay(ihp.pts,poly)
net.minor<-as.network(net[which(!is.na(index.net)),which(!is.na(index.net))],directed=FALSE)
#index<-overlay(ihp.pts,poly)



#png("test2.png",width=800,height=600,res=150)
png("test3.png",width=1600,height=1200,res=300,bg="white")
par(mar=c(1, 1, 0, 0) + 0.1,mgp=c(0, 0.2,0),tcl=-.2)
gplot.spatial(net,coord,bb=bbox(portland.proj),list.vertex=list(col=col,pch=19,cex=.0001),list.edges=list(col=rgb(0,0,0,.3),lwd=.3))
legend("topright",legend=c("Non-receivers","Seed Nodes","1st Quartile","2nd Quartile","3rd Quartile","4th Quartile"),pch=19,col=c("black","red","orange","yellow","green","blue"),cex=.5,bty="n")
par(fig=c(.25, 4, .6, 3.1)/10,new=TRUE,mgp=c(0,0.2,0),tcl=-.2)
gplot.spatial2(net,coord,bg=portland.proj,bb=bbox(poly),list.vertex=list(col=col,pch=19,cex=.0001),list.edges=list(col=rgb(0,0,0,.1)))
dev.off()

