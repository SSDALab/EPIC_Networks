 is.na.gpc.poly<-function(x){
	length(x@pts)>0
}

intersectCityLevel<-function(name,state,statefips=FALSE,level,bb.epsilon=0.01){
require(UScensus2010cdp)
require(paste("UScensus2010", level, sep = ""), character.only = TRUE)
require(gpclib)
 ###########################
 ### Helper functions
 ###########################
 
city2<-function(name,state,statefips){
	 city.ev <- new.env(parent = parent.frame())
 	 require(UScensus2010cdp)
 	 state <- check.state(state, statefips)
 	 data(list=paste(state,".cdp10",sep=""),envir =city.ev)
 	 temp.cdp<-get(paste(state,".cdp10",sep=""),envir =city.ev)
 	 name<-tolower(name)
 	 temp.cdp$name<-gsub('[^(\x20-\x7F)]*','',temp.cdp$name,perl=TRUE)
 	 
 	 if(!any(tolower(temp.cdp$name)%in%name)){stop("Name not in State!")}
 	 
 	 temp.cdp[tolower(temp.cdp$name)%in%name,]	
 }
 
 as.Polygons.gpc.poly <- function(x, ID) {
            thisPolys <- lapply(get.pts(x), function(p) {
                Polygon(rbind(as.matrix(cbind(p$x, p$y)), c(p$x[1], 
                  p$y[1])), hole = p$hole)
            })
            Polygons(thisPolys, ID)
  }
  
  buildBB<-function(city,bb.epsilon){
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
  
gpcPolygon<-function(y){
  	plys <- slot(y, "polygons")
    coords <-lapply(plys, function(x) {slot(slot(x, "Polygons")[[1]], "coords")})
        sapply(coords, as, "gpc.poly")
  }
  
gpcIntersect<-function(x,y){
	sapply(x, intersect,y[[1]])
}

  
 ###########################
 ### Helper functions
 ###########################
 


 
 if (is.null(state)) {stop("Not a State! \n")}
 city <- city2(name,state,statefips)
 state <- check.state(state, statefips)
 

 state.ev <- new.env(parent = parent.frame())
 data(list=paste(state,".",level,"10",sep=""),envir=state.ev)
 temp.level<-get(paste(state,".",level,"10",sep=""),envir=state.ev)
 
 gpclibPermit()
 bb<-buildBB(city,bb.epsilon)
 index<-overlay(SpatialPoints(coordinates(temp.level)),bb)
 if(all(is.na(index))){stop("City not in State")}
 index<-which(!is.na(index))
 
out.level<-temp.level[index, ]
gpc.level<-gpcPolygon(out.level)
city.gpc<-gpcPolygon(city)
intCityLevel<-gpcIntersect(gpc.level,city.gpc)
outIndex<-which(sapply(intCityLevel,is.na))
out.level[outIndex,]
}
