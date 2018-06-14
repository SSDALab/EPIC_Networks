## Load necessary libraries
library(multicore)
library(rgdal)
library(maptools)
library(spatstat)
library(networkSpatial)
load("sf2.rda")

####################################
#### Wrapper function for rspop
####
#### This function runs rspop one polygon at a time
#### and generates the necessary "pop" matrix based
#### on the observed households for comparability
#### e.g., Tract 1 contains 100 HH then rspop is fed 100 1 person HH
#### ... go to rspop
#### Outputs coordinates in list format (indexed by polygon)
#### in Lon/lat coordinate space
###################################
rspopWrapper<-function(polygon,...){

pop<-genpop(polygon)
coord<-vector("list",length(pop))
coordf<-vector("list",length(pop))

#coord<-mclapply(1:length(polygon@polygons),function(x){
#		print(x);
#		print(polygon$pop2000[x]);
#		poly<-polygon[x,];
#		out<-try(rspop(poly,list(pop[[x]]),projection.point=coordinates(poly),...));
		#gc();
#		out;
#		},mc.cores=15)
for(i in 1:length(coord)){
	print(i)
	poly<-polygon[i,]
	coord[[i]]<-try(rspop(poly,list(pop[[i]]),projection.point=coordinates(poly),...))
}



for(i in 1:length(coord)){
if(class(coord[[i]])!="try-error"){	
if(length(coord[[i]]$x)!=0){
projection.point=coordinates(polygon[i,])
coordf[[i]]<-SpatialPoints(cbind(coord[[i]]$x,coord[[i]]$y),proj4string=CRS(paste("+proj=ortho +lat_0=",projection.point[2], " +lon_0=", projection.point[1],collapse = "", sep = "")))
coordf[[i]]<-spTransform(coordf[[i]],CRS(proj4string(polygon)))
}
cat("Coord Change:",i,"\n")
}else{cat("Try-error:",i,"\n")}
}
coordf
}

###############
### Mod Carter's generate pop function
###############
genpop<-function(spa){
	pop<-list()
		for(i in 1:length(spa$pop2000)){
			 hpop<-c(spa$hh.1person[i],spa$hh.2person[i],spa$hh.3person[i], spa$hh.4person[i],spa$hh.5person[i],spa$hh.6person[i])
			 bpop<-spa$pop2000[i]
			if((hpop%*%(1:6))<bpop){
				cat("FYI, down by",bpop-hpop%*%(1:6),"people.  Correcting.\n")
				hpop[1]<-hpop[1]+bpop-hpop%*%(1:6)
				}
		pop[[i]]<-cbind(1:6,hpop)
		}
	pop
}

###############
### Example Usage
###############

#### Note this ignoring z-placement right now
out<-vector("list",length(sf2@polygons))
for(i in 1:length(sf2@polygons)){out[[i]]<-try(as(sf2[i,],"owin"))}
out2<-sapply(out,function(x){class(x)=="try-error"})
sf3<-sf2[which(!out2),]
sf4<-sf3[sf3$pop2000!=0,]
sf4<-sf4[-546,]
sf4<-sf4[-1024,]
sf4<-sf4[-1025,]
print("Start Pop Sim Here")
sf.pts<-rspopWrapper(sf4,method="uniform",stack.rad=20,stack.dis=4, household.jitter=5)

save(sf.pts,file="sfPts.rda")


#x<-546
#x<-8280
#poly<-sf4[x,]
#pop<-genpop(poly)
#t<-rspop(poly,list(pop[[1]]),projection.point=coordinates(poly))

which(sapply(sf.pts,function(x){any(is.null(x))}) || sf2$pop2000!=0)

sfpts<-sf.pts[[2]]
for(i in 3:length(sf.pts)){
	if(!is.null(sf.pts[[i]])){
		sfpts<-spRbind(sfpts,sf.pts[[i]])
		print(i)
	}
}

save(sfpts,file="sf_pts_sp.rda")

png("test.png")
plot(sfpts)
dev.off()


library(UScensus2000)
library(gpclib)
library(spatstat)
library(rgdal)
library(multicore)
library(networkSpatial)

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










