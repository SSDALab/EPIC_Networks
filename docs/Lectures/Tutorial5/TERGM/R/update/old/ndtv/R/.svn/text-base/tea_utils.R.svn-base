#  File R/tea_utils.R in package ndtv, part of the Statnet suite
#  of packages for network analysis, http://statnet.org .
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) at
#  http://statnet.org/attribution
#
#  Copyright 2003-2012 Statnet Commons
#######################################################################
#some basic functions to be used as a starting point for extending networkDynamic
############ SOME UTILITY METHODS FOR WORKING WITH SPELLS, ATTRIBUTES, ETC #####

#execute a network crossection, and then an attribute crossection. should be merged into network.crossection
#WARNING: behavior is not defined if multiple attributes returned
network.collapse <- function(dnet,onset,terminus=NULL, rule=c("any","all"),active.default=TRUE){
  net <- network.extract(dnet,onset=onset,terminus=terminus,rule=rule,active.default=active.default)
  #if network has zero nodes return it, but this is a bug, should still permit meta attributes
  if(length(net)>0){
    #collapse network level attributes
    activeAttrs <- list.network.attributes(net)[grep(".active",list.network.attributes(net))]
  
    for(attr in activeAttrs){
      net<-set.network.attribute(net,sub(".active","",attr),get.network.attribute.active(net,sub(".active","",attr),onset=onset,terminus=terminus))
      net<-delete.network.attribute(net,attr)
    }
    
    activeEdgeAttrs <- list.edge.attributes(net)[grep(".active",list.edge.attributes(net))]
    
    for (attr in activeEdgeAttrs){
      net<-set.edge.attribute(net,sub(".active","",attr),get.edge.attribute.active(net,sub(".active","",attr),onset=onset,terminus=terminus))
      net<-delete.edge.attribute(net,attr)
    }
    
    activeNodeAttrs <-list.vertex.attributes(net)[grep(".active",list.vertex.attributes(net))]
    for (attr in activeNodeAttrs){
      # have to decide if result should be unlisted so as not to mangle
      net<-set.vertex.attribute(net,sub(".active","",attr),get.vertex.attribute.active(net,sub(".active","",attr),onset=onset,terminus=terminus))
      net<-delete.vertex.attribute(net,attr)
    }
  }
  net
}


activate.network.attribute <- function (x, prefix, value, onset=-Inf, terminus=Inf, dynamic.only=FALSE){
  attrname = paste(prefix,"active",sep=".")
  timed <- get.network.attribute(x, attrname)[[1]]; #wierd thing where wraps all objects in lists
  if (!all(is.null(timed))){
    timed <- .append.spells(timed,value,onset,terminus)
    x <- set.network.attribute(x,attrname,value=list(timed))
    #TODO needs to check ordering of list by time values
  } else {                                  #add the new dynamic attribute
    timed <- list(vals=list(value),spls=matrix(c(onset,terminus),nrow=1,ncol=2))
    x <- set.network.attribute(x,attrname,value=list(timed))
  }
  return(x)	
}

#Only returns first value, since aggregation rules are not specified
get.network.attribute.active <- function(x,attrname,onset=-Inf,terminus=Inf,unlist=TRUE){
  #TODO: ignoreing unlist
  attributes <- get.network.attribute(x,paste(attrname,"active",sep="."))[[1]]
  valindices <- spells.hit(c(onset,terminus),attributes$spls)
  vals <- attributes$vals[[valindices]]
  return(vals)
}

#this function can be dangerous <- thanks for cryptic comment skye, wtf does this mean?
get.edge.attribute.active <- function(x,attrname,onset=-Inf,terminus=Inf,unlist=TRUE){
  #TODO: ignoreing unlist
  attributes <- get.edge.value(x,paste(attrname,"active",sep="."),unlist=F)[[1]]
  valindices <- spells.hit(c(onset,terminus),attributes$spls)
  vals <- attributes$vals[[valindices]]
  return(vals)
}

activate.vertex.attribute <- function (x, prefix, value, onset=-Inf, terminus=Inf, v=1:network.size(x), dynamic.only=FALSE){
  #possibly duplicate list for when setting multiple verticies
  #todo check for network and set nD class
  if (length(value) < length(v)){
    value <- rep(value, length=length(v))
  }
  
  if (length(onset) < length(v)){
    onset <- rep(onset, length=length(v))
  }
  
  if (length(terminus) < length(v)){
    terminus <- rep(terminus, length=length(v))
  }
  
  attrname = paste(prefix,"active",sep=".")
  timedlist <- get.vertex.attribute(x, attrname,unlist=F);
  #have to loop instead of checking if attribute exists because it can exist for some nodes and not others
  #todo: use lapply?
  for (n in seq_len(length(v))){
    timed <- .append.spells(timedlist[[v[n]]],value[n],onset[n],terminus[n])
		x <- set.vertex.attribute(x,attrname,value=list(timed),v=v[n])
	}
  #todo: return invisably and modify argument inplace
	return(x)	
}

#todo: these rules are not consistant with the spell activity rules in networkDynamic, should replace with those methods if possible
.append.spells <- function(timed,value,onset,terminus){
	#if spells are missing, create it
	if (is.null(timed) | (length(timed)==1 && is.na(timed))){  #have to check if it is null or na, but na can be long
		timed <- list(vals=list(value),spls=matrix(c(onset,terminus),nrow=1,ncol=2))
	} else {
		 #if the terminus of the previous spell is null, close it
    if(length(onset)>1) print(onset)
		nspells <- nrow(timed$spls)
		if ( (timed$spls[nspells,1] < onset) & (timed$spls[nspells,2] > onset) ){
			#close off that spell as the new one starts
			timed$spls[nspells,2] <- onset;
			#TODO if previous value is equal, and spells adjacent, should they merge
			#what if the spell to be inserted is within an existing cell?
			timed$vals <- c(timed[[1]],value)
			timed$spls <- rbind(timed$spls,c(onset,terminus))
		} else if (timed$spls[nspells,1] == onset){
			#if previous spell has equal start time, replace it, regarless of the end time
			timed$spls[nspells,2] <- terminus;
			timed$vals<-c(timed$vals[1:nspells],value)
		} else {
			#append spell and value for the new attribute
			timed$vals <- c(timed$vals[1:nspells],value)
			timed$spls <- rbind(timed$spls,c(onset,terminus))
		}
	}
	return(timed)
} 

#this may be too slow if edge allready has spells for the attribute
activate.edge.attribute <-function(x, prefix, value, onset=-Inf, terminus=Inf, 
	e=1:length(x$mel), dynamic.only=FALSE){
	attrname = paste(prefix,"active",sep=".")	
	if (attrname%in%list.edge.attributes(x)){
		for (n in e){
			timed <- get.edge.attribute(x$mel, attrname,unlist=F)[[n]];
			timed <- .append.spells(timed,value,onset,terminus)
		 	x <- set.edge.attribute(x,attrname,list(timed),e=n)
		 }
	} else {
		timed <- list(vals=value,spls=matrix(c(onset,terminus),nrow=1,ncol=2))
		x <- set.edge.attribute(x,attrname,value=list(timed),e=e)
	}
	return(x)	
}

#temporary function until the reall one is written NO CHECKING!,only returns first hit
get.vertex.attribute.active <- function(x,attrname,onset=-Inf,terminus=Inf, na.omit=FALSE,null.na=TRUE,unlist=TRUE){
  attributes <- get.vertex.attribute(x,paste(attrname,"active",sep="."),na.omit=na.omit,null.na=TRUE,unlist=FALSE)
  # some spells may not be set and will return NA
  has.spells <- which(!is.na(attributes))
  # find the index of spell matching query spell
  valindices <- sapply(has.spells,function(x){spells.hit(c(onset,terminus),attributes[[x]]$spls)})
  # kick out values for the verticies where spells don't intersect
  has.spells <- has.spells[which(valindices>0)]
  valindices <- valindices[valindices>0]
  # default to na (in case no value is defined)
 
  vals <- as.list(rep(NA,network.size(x)))  # or should this be an empty list?
  vals[has.spells] <- sapply(has.spells, function(n){attributes[[has.spells[n]]]$val[[valindices[n]]]},simplify=unlist)
  # todo: handle na.omit and null.na here!
  if (unlist){vals <-unlist(vals)}
  return(vals)
}

#takes a list of network objects and tries to gently convince them to be a dynamic network
as.dynamic.network.panel <- function(networks) {
  #TODO: add option to join adjacent spells?
  #TODO: cant assume that the networks are all the same size
  #check that everything on the list is a network and all the same size
  end <- length(networks)
  n <- network.size(networks[[1]])
  dnet <- network.copy(networks[[1]])
  #loop over networks in list and add all of the edges with the appropriate time
  for (t in 1:end){
      #TODO: check that each network is compatible with the initial network
      dnet <- activate.vertices(dnet,onset=t,terminus=t+1)
      #if the edge doesn't exist, copy it in 
      #horribly slow way to do this
      for (e in 1:length(networks[[t]]$mel)){
        #uh oh, lets pretend that there is only the possiblity of one node at each end of the list
        #and no multiplex
        existing <-get.edgeIDs(dnet,v=networks[[t]]$mel[[e]]$outl,alter=networks[[t]]$mel[[e]]$inl)
        if (length(existing)==0){
          #copy in the edge
          add.edge(dnet,networks[[t]]$mel[[e]]$outl,networks[[t]]$mel[[e]]$inl,edge.check=T)
          #TODO: do something about attributes, but need TEA
          #assume it was added to the end of the list
          existing <- length(dnet$mel)
        } 
        dnet <- activate.edges(dnet,onset=t,terminus=t+1,e=existing)
      }
      
  }
  return(dnet)
}

#plots spells of a network as a timeline for diagnostics
plot.spells <-function(x,v=1:network.size(x), e=1:length(x$mel),min.inf.replace=0,max.inf.replace=12,plot.vertex.spells=TRUE, plot.edge.spells=TRUE,
  displaylabels=TRUE,xlim=c(0,10)){
	tel <- as.data.frame(x)[c(3,4,1,2)]
	tvl <- as.tvl(x)
	tel$enddate[tel$end==Inf] <- max.inf.replace
	tel$enddate[tel$start==-Inf] <- min.inf.replace
	tvl$enddate[tvl$enddate==Inf] <- max.inf.replace
	
	v.rows <- 0 # which(tvl$vertex%in%v)
	e.rows <- union(which(tel$tail%in%v),which(tel$head%in%v))
	
	rows <- length(e.rows)*plot.edge.spells
	plot(NULL,NULL, xlim=xlim,ylim=c(1,(rows*plot.edge.spells)+(length(v.rows)*plot.vertex.spells)),xlab="time",ylab="spells")
		
	if (plot.edge.spells){
		
		junk <- sapply(1:rows, function(x){
			
			lines(c(tel$start[e.rows[x]],tel$end[e.rows[x]]),c(x,x),col=rgb(0,.2,0,.5,.5))
			if (displaylabels){
				text(tel$start[e.rows[x]],x,labels=paste("e",tel$tail[e.rows[x]],"-",tel$end[e.rows[x]]),col="green",cex=0.5)
			}
		})
		
	}
	
	#now plot nodes
	if (plot.vertex.spells){
		start <-rows+1
		end <- length(v.rows)+rows
		
		junk <- lapply(start:end, function(x){
			lines(c(tvl$startdate[v.rows[x-rows]],tvl$enddate[v.rows[x-rows]]),c(x,x),col=rgb(0,0,.2,.5))
			if (displaylabels){
				text(tvl$startdate[v.rows[x-rows]],x,labels=paste("v",tvl$vertex[v.rows[x-rows]]),col="blue",cex=0.5)
			}
		})
		rows <- length(v.rows)
	}
	
}

#function to check if two spells intersect
spells.overlap <-function(s1,s2){
  if (length(s1)!=2 | length(s2)!=2){
		stop("Each spell must be a vector of length 2")
	}
	if (( s1[1] >= s2[1]) & (s1[1]<s2[2])){
		return(TRUE)
	} else if (( s1[2] > s2[1]) & (s1[2]<=s2[2])){
		return(TRUE)
	} else if (( s1[1] <= s2[1]) & (s1[2]>=s2[2])){
		return(TRUE)
	} else {
		return(FALSE)
	}
}

#search an array of spells to see if any intersect
spells.hit<-function(needle,haystack){
  if(length(needle)!=2){
    stop("search spell (needle) must have exactly two elements")
  }
  if(ncol(haystack)!=2){
    stop("target  spell matrix (haystack) must have exactly two columns")
  }
  if(nrow(haystack)<1){
    stop("target  spell matrix (haystack) must have at least one row")
  }
  for (s in 1:nrow(haystack)){
    if(spells.overlap(needle,haystack[s,])){
      return(s)
    }
  }
  return(-1)
}


#convert dynamic network object to timed vertex list.  
#Verticies with multiple spells will have the extra spells appended at the end
as.tvl <- function(net) {
  #check that active information is attached
  noTimes <- FALSE
  if(!"active"%in%list.vertex.attributes(net)){
    warning("network verticies do not have 'active' vertex attribute to specify active spells,assuming always active")
    noTimes <-TRUE
  }
	#do all the first spells	
	tvl<- matrix(ncol=3,nrow=0)
	dimnames(tvl) <- list(NULL,c("vertex","startdate","enddate"))
  
  if(noTimes){
    #assume -Inf Inf spell
    tvl<- cbind(seq_len(network.size(net)),-Inf,Inf)
  } else {
  	#repeat for edges that may have multiple spells up to the s-th spell
  	maxspells <- max(sapply(seq_len(network.size(net)),function(x){nrow(net$val[[x]]$active)}))
  	for (s in seq_len(maxspells)){
  		splindex <- which(unlist(seq_len(network.size(net)),function(x){nrow(net$val[[x]]$active)}) >= s);
  		tvl <-rbind(tvl,t(sapply(splindex,function(x){c(x,net$val[[x]]$active[s,1],net$val[[x]]$active[s,2])})))
  	}
  }
  dimnames(tvl) <- list(NULL,c("vertex","startdate","enddate"))
	tvl <- data.frame(tvl);
	return(tvl)
}

#applies a function to all defined values in range, 
#TODO: should take range operators
aggregate.vertex.attribute.active<-function(net,attrname,fun){
  return(fun(unlist(lapply(lapply(net$val,"[[",paste(attrname,".active",sep='')),"[[","vals"))))
}
