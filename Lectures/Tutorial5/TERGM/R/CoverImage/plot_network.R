library(network)
library(diffusion)
library(sna)
library(gpclib)
library(spatstat)
library(rgdal)
library(multicore)
library(maptools)

load("sfNetwork.rda")

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
	plot.window(xlim=bb[1,],ylim=bb[2,])
	x<-as.matrix.network(x,"edgelist")
	for(i in 1:NROW(x)){
	do.call("lines",c(list(x=rbind(coord[x[i,1],],coord[x[i,2],])),list.edges))      
	}     
	do.call("points",c(list(x=coord),list.vertex))          
}

#net<-portland.net
#set.edge.attribute(net,"diffrate",1)
#city.center<-coordinates(buildBB(sf.proj,.001))
#radius<-500 #meters
#coord<-coordinates(ihp.pts)

#distM<-vector(length=nrow(coord))
#for(i in 1:length(distM)){distM[i]<-dist(rbind(city.center,coord[i,]))}
#sum(distM<500)/length(distM)
#state<-as.numeric(distM<radius)
#net%v%"diffstate"<-state
#net%v%"diffstate"<-rbinom(network.size(net),1,.3)
# start with x highest degree nodes
#net<-diffusion(net)

#save(net,file="diffusion2.rda")

#tab<-table(net%v%"diffdepth",useNA="ifany")
tab
#
#depth<-net%v%"diffdepth"
#col<-rep("",length(depth))
#col[is.na(depth)]<-"black"
#col[depth==0]<-"red"
#depth[depth==0]<-NA
#quant<-quantile(depth,na.rm=TRUE)
#colors<-c(orange,yellow,green,blue) #heat.colors(4)
# red,orange,yellow,green,blue,purple
#col[depth<=quant[2]]<-colors[1]
#col[quant[2]<depth & depth<=quant[3]]<-colors[2]
#col[quant[3]<depth & depth<=quant[4]]<-colors[3]
#col[ quant[4]<depth & depth<=quant[5]]<-colors[4]
#table(col)

png("test.png")
gplot.spatial(net,coord,bb=bbox(portland.proj),list.vertex=list(col="red",pch=19,cex=.0001),list.edges=list(col=rgb(0,0,1,.3)))
dev.off()

