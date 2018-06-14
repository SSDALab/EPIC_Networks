library(UScensus2000)
library(gpclib)
library(spatstat)
library(rgdal)
library(multicore)
library(networkSpatial)

load("sf2.rda")
load("sf_pts_sp.rda")

################################
### Projection
################################
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
################################
### Projection
################################
pp<-projection.point<-coordinates(buildBB(sf2,.0001))
sf.proj<-orthoProj(sf2,pp)
sf.pt.proj<-orthoProj(sfpts,pp)




#################
### Hagerstarand
#################
sf.net<-rnspatial(1,coordinates(sf.pt.proj),c(0.937, 0.538, 2.956),model="atpowlaw")


save(sf.pt.proj,sf.proj,sf.net,file="sfNetwork.rda")



png("test.png")
plot(sf.proj[10,],border=rgb(0,0,0,.75))
plot(sf.pt.proj,pch=19,col=rgb(1,0,0,.5),cex=.1,add=TRUE)
dev.off()





