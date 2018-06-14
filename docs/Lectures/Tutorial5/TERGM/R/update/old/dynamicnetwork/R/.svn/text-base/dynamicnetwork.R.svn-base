#convert a regular network object into dynamicnetwork
as.dynamic <- function(net,...){
 UseMethod("as.dynamic");
}

#some what dangerous
#overide is.network so that dynamic networks are networks

#checks if network does not have time information
is.network.static <- function(x){
    #check if it is a network
    if (is.network(x) & !is.dynamic(x)){
     return(TRUE);
    } else {
     return(FALSE);
    }
}

as.dynamic.network <- function(net,edge.times = c(0,1),check.renewal = TRUE,... ){

	dynamNet = NULL;
	if (class(net) == "network"){
		dynamNet <- network.copy(net);
		dynamNet <- set.network.attribute(dynamNet, "is.renewal",FALSE);
		dynamNet <- set.network.attribute(dynamNet, "dynam.attr.names",c());
		dynamNet <- set.network.attribute(dynamNet, "dyn.edge.attr.names",c());
		#make Edge Time List
		#does not deal with possible nulls in mel
		dynamNet$etl <- lapply(dynamNet$mel,function(x){return(edge.times)});
		class(dynamNet) <- c("dynamic","network");
		
	}
	return(dynamNet);
	}
	
	as.dynamic.network.series <- function(net,check.renewal = TRUE,... ){
    if (class(net) == "network.series"){
		  net <- net$networks;
		  return(as.dynamic.list(net,check.renewal));
     }
	}
	
as.dynamic.list <- 	function(net,check.renewal = TRUE, renewal = FALSE,... ){
  dynamNet = NULL;
  
  if (!is.network.static(net[[1]])){
        stop("as.dynamic.list requires a list of static network objects to convert to a network.dynamic object\n");
  }
    netSize <- network.size(net[[1]]);
		dynamNet <- network.copy(net[[1]]);
		dynamNet <- set.network.attribute(dynamNet, "is.renewal",renewal);
		dynamNet <- set.network.attribute(dynamNet, "dynam.attr.names",c());
		dynamNet <- set.network.attribute(dynamNet, "dyn.edge.attr.names",c());
		#make Edge Time List
		#does not deal with possible nulls in mel
		#assumes networks are describing descrite interval 
		dynamNet$etl <- lapply(dynamNet$mel,function(x){return(c(0,1))});
		class(dynamNet) <- c("dynamic","network");
		#step through rest of networks, looking for additions or deletions
		step <- 2;
		while (step <= length(net)){
		  #check size is the same
		  if (network.size(net[[step]]) != netSize){
		    stop("all networks to be converted must have node sets of the same size\n");
		  }
			#increment time value for all edges still existing
			stillHere <- get.edges.intersection(net[[step]],dynamNet);
			#lapply(stillHere, function(id){set.edge.end(dynamNet,id,step)});
			for (eid in stillHere){
			 #if we are checking renewal, need to check if there is a gap between
			 #the last time and the current step.
       
       if (get.edge.end(dynamNet,eid) < step-1){
         if (  get.network.attribute(dynamNet,"is.renewal"))
         {
               dynamNet <- copy.edge(dynamNet,eid,dynamNet,c(step-1,step));
               #create a new edge with same data as old, but with new times
         
          }else {
               if (check.renewal){
                 stop("input network has renewing ties but is not flaged as a renewal network");
               } else {
                 if (!get.network.attribute(dynamNet,"is.renewal")){
                 warning("input network has renewing ties, some edge duration information will be lost");
                  }
               }
         }
       } else {
        #just increment the edge time
	      dynamNet <- set.edge.end(dynamNet,eid,step);
		    }
			}
			#add any new edges
			newEdges <- get.edges.difference(dynamNet,net[[step]]);
			#lapply(newEdges, function(id){dynamNet <- copy.edge(net[[step]],id,dynamNet,
			#	c(step-1,step))});
			for (eid in newEdges){
			 dynamNet <- copy.edge(net[[step]],eid,dynamNet,c(step-1,step));
			}
			step <- step+1;
		}
	
	return(dynamNet);
}

#takes initial netowork and list of changes and constructs dynamic network

# $changed: a three column matrix (time step, head node, tail node)
#  There is one row for each changed dyad over the entire series (i.e., time
#0 to the last time). Values listed under time step i are the changes made to
#the network at time step i to make it the network at time i+1. They are
#listed in time step order.
as.dynamic.changelist <- function(changelist,startnet=NULL,nNodes=NULL, renewal=TRUE, last.time=max(changelist[,1])+1 ,subsample=NULL,... ){
     #make a dynamic network
     if (!is.null(subsample)){
      gdyn <- as.dynamic(network.initialize(length(subsample),directed=FALSE),edge.times = c(0, NA));
     } else { 
         if (!is.null(startnet)){
           gdyn <- as.dynamic(network.copy(startnet),edge.times = c(0, NA));
         } else {
            if (is.null(nNodes)){
            
               nNodes =  max(c(changelist[1,],changelist[2,]));
               warning(paste("network size not specified with nNodes argument, using ",nNodes));
           }
            #make a new network, using the max node id as a gues for the net size
           gdyn <- as.dynamic(network.initialize(nNodes),edge.times = c(0, NA));
         }
     }
     gdyn <- set.network.attribute(gdyn,"is.renewal",renewal);
     gdyn <- set.network.attribute(gdyn, "hyper",TRUE);
     onCount <- 0;
     offCount <- 0;
     dropCount <- 0;
     #for each toggle
     for (i in 1:nrow(changelist)){
        now <-  changelist[i,1];
        ego <- remap.id(changelist[i,2],subsample);
        alter <- remap.id(changelist[i,3],subsample);
        if ( (ego > 0) & (alter > 0)){
        earlyEdge <- get.edgeIDs.before(gdyn,edge.time=now,v=ego,
          alter=alter, neighborhood="out");
        if (length(earlyEdge) == 1){
           #if the interval is open, close it
           if (is.na(gdyn$etl[[earlyEdge]][2])){
             gdyn$etl[[earlyEdge]][2] <- now;
             offCount <- offCount+1;
           } else {
           #if the interval is closed, create a new edge with open interval
             ## gdyn <- copy.edge(gdyn,earlyEdge,gdyn,time2=c(now,NULL));
             gdyn <- add.edge.dynamic(gdyn,time = c(now,NA),ego,alter);
             onCount <- onCount +1;
           }
        } else {   #if there is no edge, create it with open interval
            #edge will have no attributes!
            gdyn <- add.edge.dynamic(gdyn,time = c(now,NA),ego,alter);
            onCount <- onCount+1;
        }
     } else {
        dropCount <- dropCount +1;
     }
     }
     cat(paste("on:",onCount," off:",offCount," dropped:",dropCount,"\n"));
     #need to close any open intervals...
     if (length(gdyn$etl) > 0){
     for (e in 1:length(gdyn$etl)){
      if (is.na(gdyn$etl[[e]][2])){
        gdyn$etl[[e]][2] <- last.time; 
      }
     }
     }
     
     return(gdyn);
}

#takes list of intervals in the form from, to, start, end and creates a dynamic network
#have to specify net size because ther may be isolates with noedges
as.dynamic.intervals <- function(interval.list,netsize,subsample=NULL,directed=TRUE,...){
  if (!is.null(subsample)){
    gdyn <- as.dynamic(network.initialize(length(subsample),directed=directed));
  } else {
   gdyn <- as.dynamic(network.initialize(netsize,directed=directed));
  }
  dropped <-0;
  for (r in 1:nrow(interval.list)){
    from <- remap.id(interval.list[r,1],subsample);
    to <- remap.id(interval.list[r,2],subsample);
    if ((from > 0) &(to >0)){
     gdyn <- add.edge.dynamic(gdyn,
        time=c(interval.list[r,3],interval.list[r,4]),from,to);
    } else {
      dropped <- dropped + 1;
    }                                                    
    
  }
  if (!is.null(subsample)){cat("skipped  ",dropped," out of sample relations","\n")}
  return(gdyn);

}

#gets the index of the id in the subsample, effectivly translating to neew id
remap.id <- function(id, subsample){
    if (!is.null(subsample)){
      return (max((subsample == id)*(1:length(subsample))));
  }   else {
    return(id);
  }
}

#gets the IDs of the edges in the neighborhood of the node for the given time point
get.edgeIDs.at <- function(dyn, time.point, v, alter = NULL, neighborhood = c("out", "in", "combined"),na.omit=TRUE){
  if (!is.dynamic(dyn)){
    stop("network argument must be a dynamic network");
  }
      #call the non timed version  
    #timelessNgh <- get.edgeIDs(dyn,v=v,alter=alter,neighborhood=neighborhood,na.omit=na.omit);
    timelessNgh <- get.edgeIDs(dyn,v,alter,neighborhood,na.omit);
    #check the edge timings of the results
    found <-vector();
    for (eid in timelessNgh){
      if (!is.null(dyn$etl[[eid]])){
        if ((dyn$etl[[eid]][1] <= time.point)
              & (dyn$etl[[eid]][2] > time.point)){
              found <- c(found,eid);
        }
      }
    }
    return(found);

}

#gets the IDS for the most recent edge before or intersecting with 
#the givin time
#MAY NOT MAKE SENSE FOR REAL MUTIPLEX NETWORKS
get.edgeIDs.before <- function (x, edge.time, v, alter = NULL,neighborhood = c("out", "in", "combined"), 
    na.omit = TRUE){
    candidates <- get.edgeIDs(x, v=v, alter=alter, neighborhood=neighborhood, na.omit=na.omit);
    found <- numeric(0);
    if (length(candidates) > 0){
       #candidates[1];
      for (id in candidates){
          #assume that if we find an open interval it is the most recent
          if (is.na(x$etl[[id]][2])){
             found <- id;
             break;
          } else if (x$etl[[id]][2]  < edge.time){  #if it ended before edge time
                 if (length(found) < 1 ){    #if it is first edge we've seen, use it
                    found <- id;
            # otherwise check that it is more recent than previously found edges         
                  } else if (x$etl[[id]][2] >= x$etl[[found]][2]) {    
                      found <- id;  
                  }
          }
      }
      
    } 
    return (found);
}




#returns a network object with edge set and attribute values appropriate
#for the given time range
get.slice.network <- function(dynamnet,time.point){
  if (!is.dynamic(dynamnet)){
    stop("argument must be a dynmaic,network\n");
  }
  #construct a new network
  net <- network.initialize(dynamnet$gal$"n");
  #copy network attributes
  dynamNetAttrs <- sort(names(dynamnet$gal));
  noCopy <- c("dynam.attr.names","dyn.edge.attr.names","is.renewal","mnext","n");
  for (attrName in dynamNetAttrs){
   if (!(attrName%in%noCopy)){
    net <- set.network.attribute(net,attrName,
      get.network.attribute(dynamnet,attrName));
   }
  }
  #copy node attributes
  vrtAttrs <- sapply(dynamnet$val, names);
  vrtAttrs <- sort(unique(unlist(vrtAttrs)));
  dynamVertAttrs <-  dynamnet$gal$"dynam.attr.names";
  for (attrName in vrtAttrs) {
   if (!(attrName%in%dynamVertAttrs)){
   #watch out for vertcies that don't have the var
    value <- get.vertex.attribute(dynamnet,attrName);
    if (!is.null(value)){
      net <- set.vertex.attribute(net,attrName,value); 
      }
   }
  }
  #bin dynamic node attribute
  for (attrName in dynamVertAttrs){
   for (v in 1:network.size(net)){
    if (! is.null(dynamnet$val[[v]][[attrName]])){
     timeline = as.numeric(unlist(dynamnet$val[[v]][[attrName]][,2]));
     #assume timeline is sorted
     #find index largest value <= the time we want
     index <- max((timeline <= time.point)*1:length(timeline));
     set.vertex.attribute(net,attrName,dynamnet$val[[v]][[attrName]][index,1],v=v);
    }
   }
  }
  #copy appropriate edges.
  #search etl for edges with start time <= time.point  < end time
  if (length(dynamnet$etl) >0){
  dynEdgeAttrs <-   dynamnet$gal$"dyn.edge.attr.names";
  edgeAttrs <- list.edge.attributes(dynamnet);
  #eids <- c();
  for (eid in 1:length(dynamnet$etl)){
      if (!is.null(dynamnet$etl[[eid]])){
        if ((dynamnet$etl[[eid]][1] <= time.point)
              & (dynamnet$etl[[eid]][2] > time.point)){
              #eids <- c(eids,eid);
               #add edges in new network
              #must be a safer way to get id of added edge
 	          	newID <- net$gal$mnext[1];
 	       	  # cat("newid",newID,"\n");
 	          	net <- add.edge(net, 
             dynamnet$mel[[eid]]$inl[1],dynamnet$mel[[eid]]$outl[1]);
           	#copy non-dynamic attributes
           for (attrName in edgeAttrs) {
             if (!(attrName%in%dynEdgeAttrs)){
               #watch out for vertcies that don't have the var
              value <- get.edge.attribute(dynamnet$mel[eid],attrName);
               if (!is.null(value)){
                   net <- set.edge.attribute(net,attrName,value,e=newID); 
                  }
              } 
            }
             #bin dynamic edge attributes
              for (attrName in dynEdgeAttrs){
                  if (! is.null(dynamnet$mel[[eid]][["atl"]][[attrName]])){
                   timeline = as.numeric(unlist(dynamnet$mel[[eid]][["atl"]][[attrName]][,2]));
                   #assume timeline is sorted
                   #find index largest value <= the time we want
                   index <- max((timeline <= time.point)*1:length(timeline));
                   net <- set.edge.attribute(net,attrName,
                   dynamnet$mel[[eid]][["atl"]][[attrName]][index,1],e=newID);
                  }
                 
                }
         }
      }
  }
 
 
  }
  return(net);
}

#converts a dynamic network into a regular one
#but actually just changes the class, leaving data structures intact
# should add agruments to specify if data should be removed
# also if edges should be aggregated in some way
as.network.dynamic <- function(x,...){
 class(x) <- "network";
 return(x);
}

as.static.network <- function(x,...){
  as.network.dynamic(x,...);
}

print.dynamic <- function(x,...){
cat("Time Range:",get.time.bounds(x),"\n");
 print.network(x);
 
}

plot.dynamic <- function(x,time.point=NULL,...) {
 if (!is.null(time.point)){
    plot.network.default(get.slice.network(x,time.point),
     xlab=paste("time: ",time.point),...);
 } else {
   plot.network.default(x,xlab="time: all",...);
 }
}

is.dynamic <- function(x){
	return(inherits(x, "dynamic"));
}





add.edge.dynamic <- function (x, time, tail, head, names.eval = NULL, vals.eval = NULL, 
    edge.check = FALSE) 
{
   if (!is.dynamic(x)) {
   	stop("argument must have class dynamic");
   
   } else {
   
   	if (is.null(time)){
   		stop("cannot add edge to dynamic network without time parameter");
	}
	edgeID <- x$gal$mnext[1];
    	invisible(.Call("addEdge_R", x, tail, head, names.eval, 
    		vals.eval, edge.check, PACKAGE = "network")); 
        if (length(time) == 1){
           x$etl[[edgeID]] <- c(time-1,time);
        } else if (length(time) == 2) {
           x$etl[[edgeID]] <- time;
         } else {
           stop(cat("unable to interpert time paramter for adding edge:",time));
         }
   return(x);
   }
}

#sets the start time of the specified edge in the network
set.edge.start <- function(network, edgeID, valid.time){
	#check type
	if (is.dynamic(network)){
		#check renewal
		#if (get.network.attribute(network,"is.renewal")){
		#	stop("this method does not yet handle renewal networks");
		#}
		network$etl[[edgeID]][1] <- valid.time;
    return(network);
	} else {
		stop("set.edge.start requires argument of class dynamic");
	}
}

#sets the end time of the specified edge in the network
set.edge.end <- function(network, edgeID,  valid.time){
	#check type
	if (is.dynamic(network)){
		#check renewal
		#if (get.network.attribute(network,"is.renewal")){
		#	stop("this method does not yet handle renewal networks");
		#}
		network$etl[[edgeID]][2] <- valid.time;
	  return(network);
	} else {
		stop("network argument is not of class dynamic");
	}
}

#gets the end time of the specified edge in the network
get.edge.end <- function(network, edgeID){
	#check type
	if (is.dynamic(network)){
		#check renewal
		#if (get.network.attribute(network,"is.renewal")){
		#	stop("this method does not yet handle renewal networks");
		#}
	  return(network$etl[[edgeID]][2]);
	} else {
		stop("network argument is not of class dynamic");
	}
}

#gets the end time of the specified edge in the network
get.edge.start <- function(network, edgeID){
	#check type
	if (is.dynamic(network)){
		#check renewal
		#if (get.network.attribute(network,"is.renewal")){
		#	stop("this method does not yet handle renewal networks");
		#}
	  return(network$etl[[edgeID]][1]);
	} else {
		stop("network argument is not of class dynamic");
	}
}

#creates a new edge in network2 that has the same vertex sets as 
#edge edgeID in network1, and copies attributes of the old edge to the new edge
copy.edge <- function(network1, edgeIDs, network2, time2=NULL){
 #check arsgs?
 	if((is.network.static(network1) | is.dynamic(network1)) 
   & is.network.static(network2)){
   for (edgeID in edgeIDs){
 		#must be a safer way to get id of added edge
 		newID <- network2$gal$mnext[1];
		add.edge(network2,network1$mel[[edgeID]]$inl,network1$mel[[edgeID]]$outl);
		#try to directly copy the attribute values and names
		network2$mel[[newID]]$atl <- network1$mel[[edgeID]]$atl;
	}
	} else if (is.dynamic(network2)) {
		if (is.null(time2) & is.dynamic(network1)){
		   time2 <- network1$etl[[edgeID]];
		} 
		for (edgeID in edgeIDs) {
		#must be a safer way to get id of added edge
 		newID <- network2$gal$mnext[1];
 		network2 <- add.edge.dynamic(network2,time=time2, 
        network1$mel[[edgeID]]$inl[1],network1$mel[[edgeID]]$outl[1]);
  	network2$mel[[newID]]$atl <- network1$mel[[edgeID]]$atl;
  	}
	}
	return(network2);

}



#returns the id(s) of an edge in network2 that has the same starting and ending
#sets as the edge with id edgeID in network1;
#is this faster as edge or node search?
get.matching.edgeIDs <- function(edgeID,network1, network2){
	#get the edge from network1
	edge1 <- network1$mel[[edgeID]];
	#do we have to check entire node sets, or just a single from to pair?
	if (is.hyper(network1) | is.hyper (network2)){
  	matches <- vector();
  	if (!is.null(edge1)){
  	#search all edges in network 2 for matches
  	#(could faster do it node based, but only if edges have  1 in and 1 out)
  	for ( id in  1:length(network2$mel)){
  		if (setequal(edge1$inl,network2$mel[[id]]$inl) 
  		    & setequal(edge1$outl,network2$mel[[id]]$outl)){
  			matches <- c(matches,id);
  		}
  	}
  	}
	}  else {
	  #network is not hyper so very easy
	  matches <- get.edgeIDs(as.network(network2),edge1$outl[1],edge1$inl[1],neighborhood="out");
	}
	if (length(matches) < 1){
		return(NULL);
	} else {
		return(matches);	
	}
		
}


##is.hyper <- function(x){
##get.network.attribute(as.network(x), "hyper")
##}




#returns a vector if ids of edges in network2 that do not have a matching
#edge in network1 
get.edges.difference <- function(network1,network2){
	#check args!!
	noMatches <- vector();
	id <- 1;
	while (id < length(network2$mel)){
		#check for missing edge
		if (!is.null(network2$mel[[id]])){
			matches <- get.matching.edgeIDs(id,network2,network1);
			if (is.null(matches)){
				noMatches <- c(noMatches,id);
			}
		}
		id <- id+1;
	}
	if (length(noMatches) <1){
		return(NULL);
	} else {
		return(noMatches);
	}
}

#returns a vector if ids of edges in network2 that  have a matching
#edge in network1 
get.edges.intersection <- function(network1,network2){
	#check args!!
	intersect <- vector();
	if (length(network2$mel) > 0){
	for (id in 1:length(network2$mel)){
		#check for missing edge
		if (!is.null(network2$mel[[id]])){
			matches <- get.matching.edgeIDs(id,network2,network1);
			if (!is.null(matches)){
				intersect <- c(intersect,id);
			}
		}
	}
	}
	if (length(intersect) <1){
		return(NULL);
	} else {
		return(intersect);
	}
}



#should change this to allow setting times for each vertex differntly...
set.dynamic.vertex.attribute <- function(net,attrname,value,valid.time=NULL,v=1:get.network.attribute(net,"n")){
  #check that is dynamic network
 if (!is.dynamic(net)){
           stop("adding vertex attributes with time values requires an argument of class dynamic\n");
      }
   
    if (any((v > get.network.attribute(net,"n")) | (v < 1))) 
        stop("Vertex ID does not correspond to actual vertex in set.vertex.attribute.\n")
    if (!is.list(value)) {
        if (!is.vector(value)) 
            stop("Inappropriate value given in set.vertex.attribute.\n")
        else value <- as.list(rep(value, length = length(v)))
    }
    else if (length(value) != length(v)) 
        value <- rep(value, length = length(v))
    #check if attribute is dynamic or not
    if (!is.null(valid.time)){
      timedValue <- list();
      if (!is.dynamic.vertex.attribute(net,attrname)){
          #add to list of dynamic attributes
          net <- set.network.attribute(net, "dynam.attr.names",c(get.network.attribute(net,"dynam.attr.names"),attrname));
      } 
      #it is already a timed list, so need to add to the end
      # THIS SHOULD BE DONE IN A C STRUCTURE SO THAT THE LIST
      # CAN BE MAINTAINED IN ORDER
      #print(net$val[[1]]$attrname);  THIS DOESN'T WORK BECAUSE OFSOMEMASKING PROBLEM
      currentValues <- get.vertex.attribute(net,attrname,unlist=FALSE);
      for (i in 1:length(value)){
        if (!is.na(currentValues[[v[i]]][1])){
          timedValue[[i]] <- rbind(currentValues[[v[i]]], c(value[[i]],valid.time));
          #check if are still in assending order
          if (is.unsorted(as.numeric(unlist(timedValue[[i]][,2])))){
            warning("some dynamic attribute times are out of order, results may be    unstable\n");
          } 
        } else {
         # must be the first entry for this vert, soadd the values as a (value,time) pair
          for (i in 1:length(value)){
            timedValue[[i]] <- matrix(c(value[[i]],valid.time),ncol=2) ;  
          }
        }
      } # end vertex loop
            value <- timedValue;
    invisible(.Call("setVertexAttribute_R", net, attrname, value, 
        v, PACKAGE = "network")) 
      } else {
       #there is no time information, so call the regular command
       set.vertex.attribute(net,attrname,value,v);
      }
}


#should change this to allow setting times for each edge differntly...
set.dynamic.edge.attribute <- function(net,attrname,value,valid.time=NULL,e = 1:length(net$mel)){
  #check that is dynamic network
 if (!is.dynamic(net)){
           stop("adding edge attributes with time values requires an argument of class dynamic\n");
      }
   
    #check if attribute is dynamic or not
    if (!is.null(valid.time)){
    #check that edge ids are in range
       if ((min(e) < 1) | (max(e) > length(net$mel))){ 
            stop("Illegal edge id (out of range) in set.dynamic.edge.attribute.\n")
       }

      timedValue <- list();
      if (!is.dynamic.edge.attribute(net,attrname)){
          #add to list of dynamic attributes
          net <- set.network.attribute(net, "dyn.edge.attr.names",c(get.network.attribute(net,"dyn.edge.attr.names"),attrname));
      } 
      #it is already a timed list, so need to add to the end
      # THIS SHOULD BE DONE IN A C STRUCTURE SO THAT THE LIST
      # CAN BE MAINTAINED IN ORDER
      #print(net$val[[1]]$attrname);  THIS DOESN'T WORK BECAUSE OFSOMEMASKING PROBLEM
      #need explicitly loop to deal with possiblity of nulls
      for (i in 1:length(value)){
        eid <- e[i];
        #check that edge eid exists and has time bounds in range
        eTime <- net$etl[[eid]];
        if (is.null(eTime)){
           stop(cat("unable to add edge attribute to edge ",eid,"because edge does not exist"));
        }
        if ((valid.time >= eTime[1]) & (valid.time <= eTime[2])){
             #get the current value attribute time list
              currentValue <- net$mel[[eid]]$atl[[attrname]]; #will this create masking problem?
        if (!is.null(currentValue)){  
          timedValue[[i]] <- rbind(currentValue, c(value[[i]],valid.time));
          #check if are still in assending order
          if (is.unsorted(as.numeric(unlist(timedValue[[i]][,2])))){
            warning("some dynamic edge attribute times are out of order, results may be    unstable\n");
          } 
        } else {
         # must be the first entry for this vert, soadd the values as a (value,time) pair
          for (i in 1:length(value)){
            timedValue[[i]] <- matrix(c(value[[i]],valid.time),ncol=2) ;  
          }
        }
        } else { #end edge time range check
             stop(cat("unable to add edge attribute to edge ",
             eid,"because edge is not active at time",valid.time));
        }
      } # end edge loop
            value <- timedValue;
    invisible(.Call("setEdgeAttribute_R", net, attrname, value, 
       eid, PACKAGE = "network")) 
      } else {
       #there is no time information, so call the regular command
       set.edge.attribute(net$mel,attrname,value,eid);
      }
}



is.dynamic.vertex.attribute <- function(network,attrname){
        return (attrname %in% get.network.attribute(network,"dynam.attr.names"));
}

is.dynamic.edge.attribute <- function(network,attrname){
        return (attrname %in% get.network.attribute(network,"dyn.edge.attr.names"));
}


#overide set.network.attribute to work wity dnamic netowrk
#set.network.attribute <- function (x, attrname, value) 
#{
#    if ((!is.network(x) & !is.dynamic(x))){ 
#        stop("set.network.attribute requires an argument of class network or dynamic.")
#        }
#    if (length(attrname) == 1) {
#        value <- list(value)
#    }
#    else {
#        if (is.list(value)) {
#            value <- rep(value, length = length(attrname))
#        }
#        else if (is.vector(value)) {
#            value <- as.list(rep(value, length = length(attrname)))
#        }
#        else stop("Non-replicable value with multiple attribute names in set.network.attribute.#\n")
#    }
#    invisible(.Call("setNetworkAttribute_R", x, attrname, value, 
#        PACKAGE = "network"))
#}

# overides regular version to allow setting attribute on dynamic netowrks
#set.vertex.attribute <- function (x, attrname, value, v = 1:x$gal$n) 
#{
#    if (!is.network(x) & !is.dynamic(x)) 
#        stop("set.vertex.attribute requires an argument of class network or network.dynamic.")
#    if (any((v > x$gal$n) | (v < 1))) 
#        stop("Vertex ID does not correspond to actual vertex in set.vertex.attribute.\n")
#    if (!is.list(value)) {
#        if (!is.vector(value)) 
#            stop("Inappropriate value given in set.vertex.attribute.\n")
#        else value <- as.list(rep(value, length = length(v)))
#    }
#    else if (length(value) != length(v)) 
#        value <- rep(value, length = length(v))
#    invisible(.Call("setVertexAttribute_R", x, attrname, value, 
#        v, PACKAGE = "network"))
#}

#get the start and end time for the passed (network.dynamic) object
#assumes end is always >= start
get.time.bounds <- function(x){
   bounds <- c(NA,NA);
   if (is.dynamic(x)){
    #check etl list
     bounds[1] <-min(bounds[1],min(unlist(lapply(x$etl,function(x){x[1]}))),na.rm=TRUE);
     bounds[2] <-max(bounds[2],max(unlist(lapply(x$etl,function(x){x[2]}))),na.rm=TRUE);
    #check attribute times
    for (varName in x$gal[["dynam.attr.names"]] ){
      varList <- get.vertex.attribute(x,varName,unlist=FALSE);
      for(v in varList){
        if (!is.na(v[1])){
          bounds[1] <- min(bounds[1],as.numeric(v[,2]),na.rm=TRUE);
          bounds[2] <- max(bounds[2],as.numeric(v[,2]),na.rm=TRUE);
        }
      }
    }
   } else {
     stop("get.time.bounds argument is not a dynamic network object");
   }
   return(bounds);

}

get.edges.spellmatrix <- function(net){
     if (!is.dynamic(net) ){
    stop("input to network time interval plot must be a network");
  }
  return(cbind(sapply(net$etl,function(x){x[1]},simplify=TRUE),
  sapply(net$etl,function(x){x[2]},simplify=TRUE)))
  
  
}

#gets all the spells for an ij pair
get.spells.for <- function(net,v,alter = NULL, neighborhood = c("out", "in", "combined"), 
    na.omit = TRUE){
       ids <- get.edgeIDs(as.network(net),v,alter=alter,neighborhood=neighborhood,na.omit);
        return(cbind(sapply(ids,function(x){net$etl[[x]][1]},simplify=TRUE),
  sapply(ids,function(x){net$etl[[x]][2]},simplify=TRUE)))
    
    }

plot.undirected.spells <- function(x,...){
     if (!is.dynamic(x)){
    stop("input to network time interval plot must be a dynamic network");
  }
  net <- x;
  time.range <- get.time.bounds(net);
  ymax <- net$gal$n;
  plot(NA,NA,xlab="time",ylab="network elements",main="Spell plot of edge durations by node",
  xlim=time.range,ylim=c(0,ymax));
  rowIndex <- 1;
   for (n in 1:net$gal$n){
    intervals <- get.spells.for(net,n);
    if (nrow(intervals) > 0){
    #for each of the edge's intervals
    for (et in 1:nrow(intervals)){
       lines(c(intervals[et,1],intervals[et,2]),c(rowIndex,rowIndex),type="l",col="black");
    }
    }
    rowIndex <- rowIndex+1;
    
  }
  
  
}

#shows a crude phase plot of the valid intervals for each element of the network
#UNDIRECTED FORM
plot.intervals <- function(x,time.range=NULL,show.vertex = TRUE,
 pickSlice=FALSE, showAttribute=NULL,...){
#check that network has valid.interval.data
 if (!is.dynamic(x)){
    stop("input to network time interval plot must be a dynamic network");
  }
  net <- x;
  
  #set up the plot
  if(is.null(time.range)){
  	#NEED A GOOD WAY TO GET THE MIN AND MAX TIMES
  	time.range <- get.time.bounds(net);
  }
  #check if we are going to do an interactive slice
  if (pickSlice){
     #plot a placeholder for the network image
     par(mfcol=c(2,1));
  }
  ymax <- length(net$mel);
  #if (show.vertex){
   #ymax <- ymax+network.size(net);
  #}
  #if (show.vertex | !is.null(showAttribute)) ymax <- ymax+network.size(net);
  plot(NA,NA,xlab="time",ylab="edge ids",main="Timeline plot of edges' valid intervals",
  xlim=time.range,ylim=c(0,ymax));
  rowIndex <- 1;
  #draw an interval for each node
  #if (show.vertex){
#  for ( v in 1:network.size(net)){
#    intervals <- net$val[[v]][["valid.intervals"]];
 #   for each of the nodes intervals
 #   for (vt in 1:nrow(intervals)){
#     lines(c(intervals[vt,1],intervals[vt,2]),c(rowIndex,rowIndex),type="l",col="blue");
      #ADD LABEL HERE?
 #   }
 #   rowIndex <- rowIndex +1;
 # }
#  }
  #draw an interval for each node attribute
  # if (!is.null(showAttribute)){
  # rowIndex <- 1;
  # for ( v in 1:network.size(net)){
  #  intervals <- net$val[[v]][[showAttribute]];
  #  #for each of the nodes intervals
  #  for (vt in 1:nrow(intervals)){
  #    lines(c(intervals[vt,1],intervals[vt,2]),c(rowIndex,rowIndex),type="l",col="red");
      #ADD LABEL HERE?
  #  }
  #  rowIndex <- rowIndex +1;
  #}
  #}
  
  #draw an interval for each edge
  #use offset  color or shading to indicate bi directional?
  
  # rather than going down the list, better to do by node pairs so that
  # intervals for the same pair can be drawn on the same line
  for (e in 1:length(net$etl)){
    #intervals <- net$etl[[e]]$atl[["valid.intervals"]]
    #for each of the edge's intervals
    #for (et in 1:nrow(intervals)){
       lines(c(net$etl[[e]][1],net$etl[[e]][2]),c(rowIndex,rowIndex),type="l",col="green");
    #}
    rowIndex <- rowIndex+1;
  }
  
  #if we are picking a slice, get the time to show
  if (pickSlice){
   
    #make sure we get coords off the interval plot
    par(mfg=c(1,1));
    sliceTime <- locator(n=1)$x;
    #show a virtical line on the plot at that point and label
    lines(c(sliceTime,sliceTime),c(0,rowIndex));
    #get a network for that time and plot it
    sliceNet <- get.slice.network(net,sliceTime);
    par(mfg=c(2,1));
    plot(sliceNet,object.scale=0.01,
       xlab = c("Network slice at time",round(sliceTime)));
     
    par(mfcol=c(1,1),mfg=c(1,1));
    
  }
 

}

#get a set of ids corresponding to verticies that have
#specific attribute value at a specific time
get.verts.with <- function(
  dynamnet,
  time.point,
  attr.name,
  value
){
#check type
  found <- vector();
  dynamVertAttrs <-  dynamnet$gal$"dynam.attr.names";
  
   for (v in 1:get.network.attribute(dynamnet,"n")){
  
    #check that the vertex has the attribute
    if (!is.null(dynamnet$val[[v]][[attr.name]])){
    #TODO: fix this so it will work with non-dynamic attributes
     timeline = as.numeric(unlist(dynamnet$val[[v]][[attr.name]][,2]));
     #assume timeline is sorted
     #find index largest value <= the time we want
     index <- max((timeline <= time.point)*1:length(timeline));
     #if index is zero then time is too small
     if (index >0){
      if (dynamnet$val[[v]][[attr.name]][index,1] == value){
          found <- c(found,v);
       }
     }
    }
   }
    return(found);

}

#map the unique values of the input vector to a set of color names
colorize <- function(x,values=unique(x),
colors=rainbow(length(values)),truncateAlpha=TRUE,
printLegend=TRUE,plotLegend=FALSE,map.na = TRUE){
#check that values and colors are the same length
if (length(colors) < length(values)){
  stop("lengths of colors and values vectors are not the same, each value must match with a color");
}
   if (map.na)
   {
     #make sure NA's are converted to strings 
     values <- paste(values);
     x <- paste(x);
   }
 for (colorNum in 1:length(colors)){
  
       x <- replace(x,x==values[colorNum],colors[colorNum]);
   
 }
 #in R 2.4 color codes are 8digit hexes, where the last 2 are alphas
 #this would mess up sonia, so strip 'em off
 if(truncateAlpha){
  x <- sapply(x,function(x){substr(x,1,7)},USE.NAMES=FALSE)
 }
 
 if(printLegend){
   print(t(rbind(values,colors[1:length(values)])));
 }
 if(plotLegend){
     plot(rep(0,length(values)),1:length(values),col=colors,cex=5,pch=19);
   text(rep(0,length(values)),1:length(values),values);
 }
 return(x);
}

#takes a set of booleans or binary values and maps 0->bacl, 1->red
hiliteColor <- function(values){
  values <- replace(values,values==TRUE,"red");
  values <- replace(values, values==FALSE,"black");
  return(values);
}


OldClass <- function(x) 
{
    oldClass(x);
}