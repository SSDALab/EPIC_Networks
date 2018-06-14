#  File R/export.movie.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2012 Statnet Commons
#######################################################################
#functions to generate and export movies
#require(sna)
require(networkDynamic)
require(animation)
#apply a series of network layouts to a networkDynamic object
#store the coordinates as temporal attributes on the network
compute.animation <- function(net, slice.par=NULL, animation.mode="kamadakawai", seed.coords=NULL, layout.par=list(),default.dist=NULL, verbose=TRUE){
  #check that we are dealing with the right types of objects
  #figure out what layouts we will be using
  layout.fun <- try(match.fun(paste("network.layout.animate.", animation.mode, sep = "")), silent = TRUE)
  if (class(layout.fun) == "try-error"){
      stop(paste("Error in compute.animation: no network animation layout function for animation.mode ", animation.mode))
  }
  #symatrize network
  
  # some stuff so we can modify argument
  xn <- deparse(substitute(net))
  ev <- parent.frame()
  
  #try load from network if it is missing
  if(is.null(slice.par)){
    slice.par <- get.network.attribute(net,"slice.par")
  }
  #if that doesn't work, guess
  if (is.null(slice.par)){
    slice.par <- guessSlicePar(net)
    if(verbose){
      print('No slice.par found, using:')
      print(slice.par)
    }
  }
  
  #store the slice.par on network for later use
  set.network.attribute(net,"slice.par", slice.par)
  
  #compute the set of start and end times for each slice
  #TODO: allow alternate "per change specification" or passing in a vector of times
  starts <- seq(from=slice.par$start,to=slice.par$end,by=slice.par$interval)
  ends <- seq(from=slice.par$start+slice.par$aggregate.dur,to=slice.par$end+slice.par$aggregate.dur,by=slice.par$interval)
  #TODO need more initial coord options
  if (is.null(seed.coords)){
    seed.coords = matrix(data=runif(network.size(net)*2) , ncol=2)
  }
    
  #delete any existing coords attached to the network
  delete.vertex.attribute(net,"animation.x.active")
  delete.vertex.attribute(net,"animation.y.active")
  
  coords <-seed.coords
  
  #extract crossections and apply layouts, must be in sequence to allow chaining
  for ( s in 1:length(starts)){
    #debug, print out the coordinte range
    xrange <-c(min(coords[,1]),max(coords[,1]))
    yrange <-c(min(coords[,2]),max(coords[,2]))
    
    if(verbose){
      print(paste("Calculating layout for network slice from time ",starts[s],"to",ends[s]))
    }
    #only compute the layout involving active nodes and edges
    activev <- is.active(net,starts[s],ends[s], rule=slice.par$rule,v=seq_len(network.size(net)))
    slice <- network.collapse(net,starts[s],ends[s], rule=slice.par$rule)
    

    #TODO: what to do when a slice returns an empty network? Currently empty networks are prohibited
    if (length(slice) > 0){
      #only update those coords that were calced
      newCoords <-coords[activev,] # maybe this assignment necessary to force a copy before passing to C?
      newCoords  <- layout.fun(slice,dist.mat=NULL, default.dist=default.dist, seed.coords=newCoords,layout.par=layout.par,verbose=verbose)
      coords[activev,] <- newCoords
      net <- activate.vertex.attribute(net,prefix="animation.x",onset=starts[s],terminus=ends[s],value=newCoords[,1],v=which(activev))
      net <- activate.vertex.attribute(net,prefix="animation.y",onset=starts[s],terminus=ends[s],value=newCoords[,2],v=which(activev))
    }
    
  }
  if(exists(xn, envir=ev))
    on.exit(assign(xn, net, pos=ev))
  return(invisible(net))
}

#go through the sets of coordinates attached to the network
#compute interpolation frames, and actually draw it out
#optionally save it directly to a file
render.animation <- function(net, render.par=list(tween.frames=10,show.time=TRUE,show.stats=NULL),verbose=TRUE,...){
  #check if coordinates have already been computed
  if (!all(c("animation.x.active","animation.y.active") %in% list.vertex.attributes(net))){
    net <- compute.animation(net)
  }
  #figure out what the slicing parameters were
  slice.par <- get.network.attribute(net,"slice.par")
  #TODO: how are we doing interpolation?
  
  starts <- seq(from=slice.par$start,to=slice.par$end,by=slice.par$interval)
  ends <- seq(from=slice.par$start+slice.par$aggregate.dur,to=slice.par$end+slice.par$aggregate.dur,by=slice.par$interval)
  
  #TODO: check that there are at least two frames
  
  #print some summary info as a starting frame?
  #compute some starting coords  
  slice <- network.collapse(net,starts[1],ends[1]) 
  activev <- is.active(net,starts[1],ends[1],v=seq_len(network.size(net)))
  
  
  
  #compute coordinate ranges to know how to scale plots
  xmin <- aggregate.vertex.attribute.active(net,"animation.x",min)
  xmax <- aggregate.vertex.attribute.active(net,"animation.x",max)
  ymin <- aggregate.vertex.attribute.active(net,"animation.y",min)
  ymax <- aggregate.vertex.attribute.active(net,"animation.y",max)
  if (!exists('xlim')){
    xlim<-c(xmin,xmax)
  }
  if(!exists('ylim')){
    ylim<-c(ymin,ymax)
  }
  
  
  #if the first slice is empty, just start from zeros
  coords<-matrix(0,ncol=2,nrow=network.size(net))
  if (length(slice)>0){ 
    #coords[activev,1] <-get.vertex.attribute.active(slice,"animation.x",onset=starts[1],terminus=ends[1])
    #coords[activev,2] <-get.vertex.attribute.active(slice,"animation.y",onset=starts[1],terminus=ends[1])
    coords[activev,1] <-get.vertex.attribute(slice,"animation.x")
    coords[activev,2] <-get.vertex.attribute(slice,"animation.y")
    #need to update plot params with slice-specific values
    if(!exists('label')){
      slice.label <- network.vertex.names(slice)
    } else {
      slice.label <- label
    }
    if(render.par$show.time){
      xlab <- paste("t=",starts[1],"-",ends[1],sep='')
    }
    if(!is.null(render.par$show.stats) && render.par$show.stats!=FALSE){
      # evaluate a eqn string giving the stats formual
      stats <- eval(parse(text=paste("summary.statistics.network(network.extract(net,onset=",starts[1],", terminus=",ends[1],")",render.par$show.stats,")",sep=''))) 
      xlab <- paste(xlab,paste(rbind(names(stats),stats),collapse=":"))
    }
    if (!exists('xlab')){
      xlab<-NULL
    }
    
    plot.network(slice,coord=coords[activev,],
                 label=slice.label,xlim=xlim,ylim=ylim,xlab=xlab,...)
  }# end slice > 0 block
    
  coords2 <- coords
  oopts = ani.options(interval = 0.1,ani.type="jpeg",ani.dev="jpeg")
  ani.record(reset=TRUE)
  #move through frames to render them out
  for(s in 1:length(starts)){
    if (verbose){print(paste("rendering",render.par$tween.frames,"frames for slice",s-1))}
    slice <- network.collapse(net,starts[s],ends[s])
    activev <- is.active(net,starts[s],ends[s],v=seq_len(network.size(net)))
   
    #TODO: draw new slices for intermediate tween frames?
    #skip any empty networks
    if (length(slice)>0){
      if(!exists('label')){
        slice.label = network.vertex.names(slice)
      } else {
        slice.label = label
      }
      
      #show the time on the plot
      if(render.par$show.time){
        xlab <- paste("t=",starts[s],"-",ends[s],sep='')
      }
      
      #show stats as title of the plot
      if(!is.null(render.par$show.stats) && render.par$show.stats!=FALSE){
        # evaluate a eqn string giving the stats formual
        stats <- eval(parse(text=paste("summary.statistics.network(network.extract(net,onset=",starts[s],", terminus=",ends[s],")",render.par$show.stats,") ",sep='')))
        xlab <- paste(xlab,paste(rbind(names(stats),stats),collapse=":"))
      }
      
      if (!exists('xlab')){
        xlab<-NULL
      }
   
      for(t in 1:render.par$tween.frames){
        #coords2[activev,1]<-get.vertex.attribute.active(slice,"animation.x",onset=starts[s],terminus=ends[s])
        #coords2[activev,2]<-get.vertex.attribute.active(slice,"animation.y",onset=starts[s],terminus=ends[s])
        coords2[activev,1]<-get.vertex.attribute(slice,"animation.x")
        coords2[activev,2]<-get.vertex.attribute(slice,"animation.y")
       # tweenCoords <- coords + ((coords2-coords)*(t/render.par$tween.frames))
        tweenCoords <- coord.interp.smoothstep(coords,coords2,t,render.par$tween.frames)
         #TODO:what if we want to include innactive nodes
        plot.network(slice,coord=tweenCoords[activev,],
                 label=slice.label,xlab=xlab,xlim=xlim,
                 ylim=ylim,...) 
        ani.record();
      }
      coords<-coords2;
    } else { # end slice > 0 block
      # draw some blank frames while time passes
      if(render.par$show.time){
        xlab <- paste("t=",starts[s],"-",ends[s],sep='')
      }
      if (!exists('xlab')){
        xlab<-NULL
      }
      singlenet <-network.initialize(1)
      for(t in 1:render.par$tween.frames){
        plot.network(singlenet,
                   vertex.cex=0,xlim=xlim,ylim=ylim,xlab=xlab)
        ani.record();
      }
    } # end empty network block
  }
  
}
    
#common function called to construct the distance matrix for mds-based layouts
layout.distance <-function(net,default.dist=NULL){
  if (is.null(default.dist)){
    default.dist=sqrt(network.size(net))
  }
  dg <- geodist(symmetrize(as.sociomatrix(net)),inf.replace=default.dist)$gdist
  return(dg)
}

#function to generate a symetrix matrix using an arbirary funciton SLOW
matrix.symmetrize <- function(mat,fun){
  for (i in seq_len(nrow(mat))){
    j<- i+1;
    while(j <=ncol(mat)){
      value <- fun(c(mat[i,j],mat[j,i]))
      mat[i,j] <- value
      mat[j,i] <- value
      j <- j+1;
    }
  }  
  return(mat)
}

guessSlicePar <- function(nd){
  times <- sort(unique(c(lapply(nd$mel, function(e) e$atl$active), 
                         recursive = TRUE)))
  # ignore inf values
  times[times==Inf]<-NA
  times[times==-Inf]<-NA
  
  # TODO: should try to guess if it is discrete or cont
  # TODO: should try to pick no more than 100 samples
  slice.par<-list(start=min(times,na.rm=T),end=max(times,na.rm=T),interval=1, aggregate.dur=1,rule="any")
  return(slice.par)
}


