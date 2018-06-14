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

	
for(i in 1:length(pop)){

	poly<-try(as(as(polygon[i,],"owin"),"SpatialPolygons"))
	if(class(poly)=="try-error"){
	poly<-polygon[i,]	
	poly@polygons[[1]]@Polygons[[1]]@coords<-unique(poly@polygons[[1]]@Polygons[[1]]@coords)
	poly<-as(as(poly,"owin"),"SpatialPolygons")
	}

coord[[i]]<-try(rspop(poly,list(pop[[i]]),projection.point=coordinates(poly),...))
cat("Polygon",i,"/",length(pop),"\n")
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

sf.pts<-rspopWrapper(sf2,method="uniform",stack.rad=10,stack.dis=4, household.jitter=5)
save(sf.pts,file="sfPts.rda")


which(sapply(sf.pts,function(x){any(is.null(x))}))

sfpts<-sf.pts[[12]]
for(i in 13:length(sf.pts)){
	if(!is.null(sf.pts[[i]])){
		sfpts<-spRbind(sfpts,sf.pts[[i]])
		print(i)
	}
}

save(sfpts,file="sf_pts_sp.rda")

png("test.png")
plot(sfpts)
dev.off()










