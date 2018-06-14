library(UScensus2000)
library(gpclib)
library(spatstat)
library(rgdal)
library(multicore)
library(networkSpatial)
source("inhomPointProcessMatrixDist.R")
load("portland.rda")

#######################
## Generate point by point block data
#######################

crs<-CRS(proj4string(portlandBlk))
ptownList<-lapply(portlandBlk@polygons,function(x){SpatialPolygons(list(x),proj4string=crs)})

pointsPerBlock<-mclapply(ptownList,function(x){
index<-overlay(portland.pts,x)
portland.pts[which(!is.na(index)),]
},mc.cores =10)

n<-sapply(pointsPerBlock,function(x){nrow(slot(x,"coords"))
})

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
        SpatialPolygons(list(temp.pb), proj4string = CRS("+proj=longlat +datum=NAD83"))
  }

orthoProj<-function(polygon,pp){
	#projection.point<-coordinates(buildBB(polygon,.0001))
	projection.point<-pp
	trans<-CRS(paste("+proj=ortho +lat_0=",projection.point[2], " +lon_0=", projection.point[1],collapse = "", sep = ""))
	spTransform(polygon,trans)
}


#polyBlk<-portlandBlk
#Blk<-polyBlk[which(!is.na(index)),]
pp<-projection.point<-coordinates(buildBB(portlandBlk,.0001))
portland.ihp<-vector("list",length(ptownList))

for(i in 1:length(portland.ihp)){
Blk<-ptownList[[i]] 
Blk<-orthoProj(Blk,pp)

suppressWarnings(test.ppf<-pointpotfield(40,bounding.poly=Blk,attracting.poly=Blk,dist.scale=0.00015,dist.exp=-1.5,return.mode="im",cores=15))

if(n[[i]]==1){portland.ihp[[i]]<-coordinates(Blk)
	}else if(length(Blk@polygons[[1]]@Polygons)>1){
		
		for(k in 2:length(Blk@polygons[[1]]@Polygons)){Blk@polygons[[1]]@Polygons[[k]]<-NULL}
		
		portland.ihp[[i]]<-rppinhom(n[[i]],bounding.poly=Blk,ppf.precomp=test.ppf,sim.mode="rasterized",stack.rad=1.5,stack.dis=1,plot.mode="max",stack=TRUE)
	}else{
portland.ihp[[i]]<-rppinhom(n[[i]],bounding.poly=Blk,ppf.precomp=test.ppf,sim.mode="rasterized",stack.rad=1.5,stack.dis=1,plot.mode="max",stack=TRUE)
}
print(i)
if(i%%100==0){save(portland.ihp,file="portland.ihp.rda")}
}


save(portland.ihp,n,ptownList,file="portland.ihp.rda")



############
## Plot information
############

#buildGrid<-function(polygon){
#grd <- gridlines(polygon)
#grdat <- gridat(polygon)
#list(grid=grd,gridInfo=grdat)
#}

#bldGrd<-buildGrid(poly[1,])

#png(file="portlandTractEX1.png",width=600,height=600,res=100)
#par(mar=c(0,0,0,0) + 0.1)
#plot(bldGrd$grid,lty=2)
#plot(poly[1,],add=TRUE)
#plot(polyBlk[which(!is.na(index)),],add=TRUE,border="gray")
#plot(pts[[1]],pch=19,cex=.2,add=TRUE,col=rgb(.8,0,0,.99))
#plot(bldGrd$grid,add=TRUE,lty=2)
#text(coordinates(bldGrd$gridInfo), labels=parse(text=as.character(bldGrd$gridInfo$labels)),
#   pos=bldGrd$gridInfo$pos, offset=bldGrd$gridInfo$offset,cex=.75)
#dev.off()