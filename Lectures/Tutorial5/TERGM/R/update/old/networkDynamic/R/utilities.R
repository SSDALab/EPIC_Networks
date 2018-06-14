#  File networkDynamic/R/utilities.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team

################################################
# utilities.R
# Author: Zack W. Almquist, Pavel , Li Wang lxwang@uw.edu
#
# Includes:
#  	     c.network #overloaded c operator for network objects
#
#	misc helper functions not for general use
#
################################################



# see https://statnet.csde.washington.edu/trac/wiki/NetworkDynamicConverterFunctions
# for full specs on this constructor function

networkDynamic <- function(base.net=NULL,edge.toggles=NULL,vertex.toggles=NULL,
                  edge.spells=NULL,vertex.spells=NULL,edge.changes=NULL,vertex.changes=NULL,
                  network.list=NULL,onsets=NULL,termini=NULL,vertex.pid=NULL,start=NULL,end=NULL,net.obs.period=NULL,...) {
  
  
  if (!is.null(start) && !is.null(end)) {
    if (start > end) stop ("start must be less than end")
  }
  if (!is.null(start) || !is.null(end)) {
    if (!is.null(net.obs.period)) stop("net.obs.period can not be specified with start and end arguments")
  }
  if (!is.null(base.net)) {
    if (!is.network(base.net)) stop("base.net must be either NULL or a network object")
  }
  
  if (is.null(network.list) & (!is.null(onsets) || !is.null(termini))) {
    warning("Onsets and termini arguments only used when a network.list is provided.")
  }
  
  # ------- network.list specified ---------
  if (!is.null(network.list)) {
    # ---- check parameters ----
    if (!is.null(vertex.toggles) || !is.null(vertex.spells) ||!is.null(vertex.changes) ||
      !is.null(edge.toggles) || !is.null(edge.spells) ||!is.null(edge.changes)) {
      stop("Vertex or edge activation arguments can not be used along with a network.list")
    }
    
    if (!is.null(onsets)) {
      if (!(is.numeric(onsets) && (length(onsets) == length(network.list)))) {
        stop("Onsets and termini must be NULL or numeric, and the same length as network.list")
      }
    }
    if (!is.null(termini)) {
      if (!(is.numeric(termini) && (length(termini) == length(network.list)))) {
        stop("Onsets and termini must be NULL or numeric, and the same length as network.list")
      }
    }
    
    net.sizes = sapply(network.list, function(y){if (is.network(y)) network.size(y) else NA})
    if (sum(sapply(net.sizes, is.na)) > 0) {
      stop("All elements of network.list must be network objects")
    }
    
    net.attribs = c('directed', 'hyper', 'loops', 'multiple', 'bipartite')
    temp = do.call(rbind, lapply(network.list, 
                                 function(x) {
                                    sapply(net.attribs, function(a) get.network.attribute(x, a))}))
    if (!all(sapply(seq_len(ncol(temp)), function(x) length(unique(temp[,x])))==1))
      warning("Networks in network.list have different attributes. Only the first network's attributes are retained.")
    
    if (length(unique(net.sizes)) != 1) {
      if (is.null(vertex.pid)) {
        stop("vertex.pid must be specified when network sizes of network.list are different")
      }
    }
    
    if (!is.null(vertex.pid)) {
      if (vertex.pid %in% list.vertex.attributes(network.list[[1]])) {
        # placeholder
      } else {
        stop("vertex.pid must be present in the network vertex attributes")
      }
    }
    
    # observation period
    if (!is.null(start) && !is.null(onsets)) {
      stop("only one of start and onsets should be specified")
    }
    
    if (is.null(net.obs.period)) {
      # end is not used. if start is specified, assume end to be start + length
      
      if (is.null(start) && is.null(onsets)) {
        start = 0;
      }
      # only start
      if (is.null(onsets)) {
        net.obs.period = list(observations=list(c(start,start+length(network.list)),mode="discrete", time.increment=1,time.unit="step"))
        onsets=seq(from=start, length=length(network.list))
        termini=seq(from=start, length=length(network.list)) + 1
      } else {
        # only onsets
        if (is.null(termini)) stop("onsets and termini must be specified together")
        obs = lapply(1:length(termini), function(i) c(onsets[i], termini[i]))
        net.obs.period = list(observations=obs, mode='continuous')
        
      }
        
    } else {
       
    }
    
    
    
    # ---- vertex.pid present ----
    if (!is.null(vertex.pid)) {
      # build base network vertices <-> pids
      base.net.pids = NULL      
      for (i in seq_along(network.list)) {
        if (vertex.pid %in% list.vertex.attributes(network.list[[i]])) {
          net.pids = get.vertex.attribute(network.list[[i]], vertex.pid)
          if (!is.unique.list(net.pids)) 
            stop("vertex.pid attribute must be unique for each vertex.")
          base.net.pids = c(base.net.pids, net.pids)
        } else {
          stop("vertex.pid must be present in the network vertex attributes for each network")
        }
      }  
      base.net.pids = sort(unique(base.net.pids))
      
      # initialize, copy network attributes
      if (is.null(base.net)) base.net = network.list[[1]]
      out.net = network.initialize(length(base.net.pids), directed = base.net%n%"directed", 
                                   hyper = base.net%n%"hyper", loops = base.net%n%"loops", 
                                   multiple = base.net%n%"multiple", bipartite = base.net%n%"bipartite")
      set.vertex.attribute(out.net, vertex.pid, base.net.pids)
      base.net <- out.net
      
      # get combined edge list, indexed by the vertices in base.net
      for (i in seq_along(network.list)) {
        edgelist = as.edgelist(network.list[[i]])
        net.pids = get.vertex.attribute(network.list[[i]], vertex.pid)
        # convert the network vertex indices to base.net indices, using pids
        edges = apply(edgelist, c(1,2), function(x) {
          which(base.net.pids == net.pids[x])[1]
        })
        
        # activate the vertices
        vertices<-match(net.pids, base.net.pids)
        os<-rep(onsets[i], length(vertices))
        ts<-rep(termini[i], length(vertices))
        activate.vertices(base.net,onset=os,terminus=ts,v=vertices)
        
        # activate the edges
        for (e in seq_len(nrow(edges))) {
          t = edges[e,1]; h = edges[e,2]
          # add edge if necessary
          if (length(get.edgeIDs(base.net, t, h)) == 0) {
            add.edge(base.net, tail=t, head=h)
          }
          activate.edges(base.net, e = get.edgeIDs(base.net,t,h)[1], 
                         onset=onsets[i], terminus=termini[i]) 
          
        }
      }
      
    } else {
      # ---- no vertex.pid, all networks same size ----
      
      # initialize, copy network attributes
      if (is.null(base.net)) base.net = network.list[[1]]
      out.net = network.initialize(network.size(network.list[[1]]), directed = base.net%n%"directed", 
                                   hyper = base.net%n%"hyper", loops = base.net%n%"loops", 
                                   multiple = base.net%n%"multiple", bipartite = base.net%n%"bipartite")
      # copy vertex attributes!!!
      
      base.net <- out.net
      
      # get combined edge list, indexed by the vertices in base.net
      for (i in seq_along(network.list)) {
        edges = as.edgelist(network.list[[i]])
        
        # all vertices are assumed to be active
        activate.vertices(base.net, onset=onsets[i], terminus=termini[i])
        
        # activate the edges
        for (e in seq_len(nrow(edges))) {
          t = edges[e,1]; h = edges[e,2]
          # add edge if necessary
          if (length(get.edgeIDs(base.net, t, h)) == 0) {
            add.edge(base.net, tail=t, head=h)
          }
          activate.edges(base.net, e = get.edgeIDs(base.net,t,h)[1], 
                         onset=onsets[i], terminus=termini[i]) 
          
        }
      }
      
    }
    
    set.network.attribute(base.net, "net.obs.period", net.obs.period)
    # end of network list
    
    # ---------- edge or vertex timings specified  -----------
  } else {
    # ---- check parameters ----
    if (!is.null(network.list)) {
      stop("Vertex or edge activation arguments can not be used along with a network.list")
    }
    vertex.args = list(vertex.toggles, vertex.changes, vertex.spells)
    vertex.which = which(!sapply(vertex.args, is.null))
    if (length(vertex.which) > 1) 
      stop("Only one of vertex.toggles, vertex.spells and vertex.changes should be specified.")
    # pick out the non-null argument, if it is given
    vertex.data = (if (length(vertex.which)==1) vertex.args[[vertex.which]] else NULL)
    
    if (!is.null(vertex.data)){
      vertex.data = as.matrix(vertex.data)
      if (!is.null(vertex.changes)) {
        if (ncol(vertex.data) < 3) stop("vertex.changes requires 3 columns: time, vertex.id, direction")
        colnames(vertex.data)[1:3] = c("time", "vertex.id", "direction")
      }
      if (!is.null(vertex.toggles) ) {
        if (ncol(vertex.data) < 2) stop("vertex.toggles requires 2 columns: time, vertex.id")
        colnames(vertex.data)[1:2] = c("time", 'vertex.id')
      }
      
      if (!is.null(vertex.spells)) {
        if (ncol(vertex.data) < 3) stop("vertex.spells requires 3 columns: onset, terminus, vertex.id") 
        colnames(vertex.data)[1:3] = c('onset', 'terminus', 'vertex.id')
      }
    }
        
    edge.args = list(edge.toggles, edge.changes, edge.spells)
    edge.which = which(!sapply(edge.args, is.null))
    if (length(edge.which) > 1)
      stop("Only one of edge.toggles, edge.spells and edge.changes should be specified.")
    edge.data = (if (length(edge.which)==1) edge.args[[edge.which]] else NULL)
    
    if (!is.null(edge.data)) {
      edge.data = as.matrix(edge.data)
      if (!is.null(edge.changes)) {
        if (ncol(edge.data) < 4) stop("edge.changes requires 4 columns: time, tail, head, direction")
        colnames(edge.data)[1:4] = c('time', 'tail', 'head', 'direction')
      }
      if (!is.null(edge.toggles)) {
        if (ncol(edge.toggles) < 3) stop("edge.toggles requires 3 columns: time, tail, head")
        colnames(edge.data)[1:3] = c('time', 'tail', 'head')
      }
      if (!is.null(edge.spells)) {
        if (ncol(edge.spells) < 4) stop("edge.spells requires 4 columns: onset, terminus, tail, head")
        colnames(edge.data)[1:4] = c('onset', 'terminus', 'tail', 'head')
      }
    }
    
    if (is.null(edge.data) && is.null(vertex.data)) print('both edge and vertex not specified')
    
    # ---- initialize base.net ----
    # fill in base network if it is not given
    max.vertex = max(vertex.data[,'vertex.id'], edge.data[,'tail'], edge.data[,'head'])
    if (is.null(base.net)) base.net = network.initialize(max.vertex)
    
    if (is.null(net.obs.period)) {
      # observation.period and censoring
      if (is.null(start)) start = mintime(vertex.data, edge.data)
      if (is.null(end)) end = maxtime(vertex.data, edge.data)
      
      net.obs.period = list(observations = c(start, end))
      if (!is.null(edge.spells) || !is.null(vertex.spells)) {
        net.obs.period$mode = 'continuous'
      } else {
        net.obs.period$mode = 'discrete'
        net.obs.period$time.increment=1
        net.obs.period$time.unit="step"
        
      }
    } else {
      if (!is.null(start)) stop("start and end should not be specified with net.obs.period")
    }
    set.network.attribute(base.net, "net.obs.period", net.obs.period)
    
    # strict construction for now
    if (max.vertex > network.size(base.net)) stop("base.net network size is smaller than size implied by vertex.ids in vertex or edge argument")
    
    # remove any activity from base.net (for now)
    delete.vertex.activity(base.net)
    if (network.edgecount(base.net) > 0) delete.edge.activity(base.net)
    
    
    # ---- vertex data ----
    if (!is.null(vertex.data)){
      # sort by time
      vertex.data = vertex.data[order(vertex.data[,1]), ]
      
      # initialize
      if (!is.null(vertex.toggles)) activate.vertices(base.net, onset=-Inf, terminus=Inf)
      
      if (!is.null(vertex.spells)) {
        activate.vertices(base.net, v=vertex.data[,'vertex.id'], onset=vertex.data[,'onset'], terminus=vertex.data[,'terminus'])
      } else {
        for (i in seq_len(nrow(vertex.data))) {
          # todo: maybe do this in try catch so we can give appropriate line numbers for errors?
          at = vertex.data[i,'time']
          v = vertex.data[i,'vertex.id']
          change.activate = (if (is.null(vertex.changes)) !is.active(base.net, at=at, v=v) else vertex.data[i,'direction']==1) 
          if (change.activate) {
            activate.vertices(base.net, v=v, onset=at, terminus=Inf)
          } else {
            deactivate.vertices(base.net, v=v, onset=at, terminus=Inf)
          }
        }
      }
    }
    
    # ---- edge data ----
    if (!is.null(edge.data)) {        
      # sort by time
      edge.data = edge.data[order(edge.data[,1]), ]
      
      # initialize
      #if (is.null(edge.spells)) activate.edges(base.net, onset=-Inf, terminus=Inf)
      # assume edges in base.net to be active initially
      if (!is.null(edge.toggles)) activate.edges(base.net, onset=-Inf, terminus=Inf)
      
      for (i in seq_len(nrow(edge.data))) {
        t = edge.data[i,'tail']; h = edge.data[i,'head']
        e = get.edgeIDs(base.net, t, h)
        # add edge if not present in the base.net (as inactive?)
        if (length(e) == 0) {
          add.edge(base.net, t, h)
          e = get.edgeIDs(base.net, t, h)
          if (!is.null(edge.toggles)) deactivate.edges(base.net, e=e, onset=-Inf, terminus=Inf)
        }
        
        if (!is.null(edge.spells)) {
          activate.edges(base.net, e=e, onset=edge.data[i,'onset'], terminus=edge.data[i,'terminus'])
        } else {
          at = edge.data[i,'time']
          change.activate = (if (!is.null(edge.toggles)) !is.active(base.net, at=at, e=e) else edge.data[i,'direction']==1) 
          if (change.activate) {
            activate.edges(base.net, e=e, onset=at, terminus=Inf)
          } else {
            deactivate.edges(base.net, e=e, onset=at, terminus=Inf)
          }
        }
      }
    }
    
  }
  
  # if only base net is specified, set.nD.class on it and return. 
  return(set.nD.class(base.net))
  
  
  # temporariy kludge, call other hidden package functions
  #  if (!is.null(edge.toggles) & !is.null(base.net)){
  #    if (is.null(start)){
  #      start<-min(edge.toggles[,1])-1
  #    }
  #    if (is.null(end)){
  #      end<-max(edge.toggles[,1])
  #    }
  #    return (as.networkDynamic.network(base.net,toggles=edge.toggles,start=start,end=end))
  #    
  #  }
  
  
}

################
### start networkDynamic-> other formats
################

# Get activity functions

# wrapper functions to return activity matrices of edges and vertices
get.edge.activity <- function(x, e=seq_along(x$mel), as.spellList=FALSE) {
  if(length(x$mel)>0) 
    if((min(e) < 1) || (max(e) > x%n%"mnext"-1)) 
      stop("Illegal edge in get.edge.activity.\n")
  
  if (as.spellList) {
    return(as.data.frame.networkDynamic(x, e=e))
  } else {
    return(get.edge.attribute(x$mel[e], "active", unlist=FALSE))
  }
  
}

get.vertex.activity <- function(x, v=seq_len(network.size(x)), as.spellList=FALSE) {
  if((min(v) < 1) || (max(v) > network.size(x))) 
    stop("Illegal vertex in get.vertex.activity.\n")  
  vam=get.vertex.attribute(x, "active", unlist=FALSE)
  
  if (as.spellList) {
    get.vertex.spelllist(x, node.list=vam[v])
  } else {
    vam[v]
  }
}



# tail and head are nodeIDs
as.data.frame.networkDynamic<-function(x, row.names = NULL, optional = FALSE,e=seq_along(x$mel), start=NULL, end=NULL, ...){
  if(is.null(start) && !is.null(attr(x,"start"))) start <- attr(x,"start")
  if(is.null(end) && !is.null(attr(x,"end"))) end <- attr(x,"end")
  
  tm<-lapply(x$mel,function(y){
    if(is.null(y)) NULL else{
      active<-y$atl$active
      if (!is.null(active)) {
        ac<-matrix(rep(cbind(y$outl,y$inl),nrow(active)),ncol=2,byrow=TRUE)
        cbind(active,ac)
      }
    }
  })
  out <- do.call(rbind,tm)
  if (is.null(out)) {
    out = data.frame(onset=numeric(), terminus=numeric(), tail=numeric(), head=numeric())
    warning("Object does not have any edge activities")
  } else {
    colnames(out)<-c("onset","terminus","tail","head")
  }
  out<-data.frame(out)
  
  out$onset.censored <- out$onset==-Inf
  out$terminus.censored <- out$terminus==Inf
  
  if(!is.null(start)) out$onset[out$onset.censored] <- start
  
  if(!is.null(end)) out$terminus[out$terminus.censored] <- end
  
  out$duration <- out$terminus-out$onset  
  
  # edge ids
  eids = mapen(x,out[,3:4])
  out$edge.id = eids
  
  out = out[eids %in% e,]
  out = out[order(out$onset),]
  # sort output by eid, onset,terminus
  out<-out[order(out[,8],out[,1],out[,2]),]
  return(out)
}

# return [onset, terminus, vertex]
get.vertex.spelllist = function (x, node.list=NULL, start=NULL, end=NULL) {
  # todo: need to replace this with reading net.obs.period attribute
  if(is.null(start) && !is.null(attr(x,"start"))) start <- attr(x,"start")
  if(is.null(end) && !is.null(attr(x,"end"))) end <- attr(x,"end")
  
  
  if (is.null(node.list)) node.list = get.vertex.activity(x)
  
  out = lapply(seq_along(node.list), function(i) cbind(node.list[[i]], i))
  out = do.call(rbind, out)
  
  if (is.null(out)) {
    out = data.frame(onset=numeric(), terminus=numeric(), tail=numeric(), head=numeric())
    warning("Object does not have any edge activities")
  } else {
    colnames(out) = c("onset", "terminus", "vertex")
  }
  
  out = data.frame(out)
  out = out[order(out$onset, out$vertex),]
  out
}

################
### end networkDynamic-> other formats
################

print.networkDynamic <- function(x, ...){
  #times <- sort(unique(c(lapply(x$mel, function(e) e$atl$active),recursive=TRUE)))
  #cat("networkDynamic with", length(times), "distinct change times:\n")
  #print(times)
  NextMethod("print")
}


##############
### as.networkDynamic
### converts various objects to networkDynamic
##############

is.networkDynamic <- function(x){
  "networkDynamic" %in% class(x)
}

as.networkDynamic <- function(object,...){
  .Deprecated("networkDynamic()",old="as.networkDynamic.network",msg="networkDynamic converter and data merge functions are being refactored to avoid `as.networkDynamic.*' methods and S3 calls.  See networkDynamic function instead. In the future as.networkDynamic will only set a networkDynamic class on its argument.")
  UseMethod("as.networkDynamic")
}

# doesn't do anything. Returns object as is
as.networkDynamic.networkDynamic <- function(object,...){
  return(object)
}


# if x is an nD object, return x
# otherwise, do something weird to it??
set.nD.class <- function(x){
  if(!is.networkDynamic(x)) {
    xn <- deparse(substitute(x))
    ev <- parent.frame()
    class(x) <- c("networkDynamic", class(x))
    if(exists(xn, envir=ev))
      on.exit(assign(xn, x, pos=ev))
    return(invisible(x))
  }
  return(x)
}

as.edgelist <- function(nw, attrname = NULL, as.sna.edgelist = FALSE,...){
  el <- as.matrix.network.edgelist(nw, attrname=attrname, as.sna.edgelist=as.sna.edgelist,...)
  if(!is.directed(nw)) el[,1:2] <- cbind(pmin(el[,1],el[,2]),pmax(el[,1],el[,2]))
  el
}

# given a network object (or networkDynamic) and a vertex name, return the vertex id
# net: network or networkDynamic object
# vertex.name: the vertex name to look up id for. Usually string type
# returns the internal id of the vertex name. Gives a warning if the name isn't unique.
get.vertex.id = function (net, vertex.name) {
  if (!is.network(net)) stop("Error: argument is not a network object")
  temp = which(network.vertex.names(net) == vertex.name)
  if (length(temp) == 0) stop("Error: vertex name not found")
  if (length(temp) > 1) warning("Warning: vertex names are not unique!")
  return(temp[1])
}

# given a vector or list, return true if it is unique
is.unique.list = function(x) {
  length(x) == length(unique(x))
}

#### Builds edge index map 
# net: base network for the nD object. 
# e: edgelist with vertices numbered according to the ones in net
# returns the edge id's of the edges given by e, in net
mapen<-function(net,e){
  apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})
}

## returns the min and max times of vertex and edge timings
mintime = function(vertex.data, edge.data) {
  if ('time' %in% colnames(vertex.data)) t1 = 'time' else t1 = 'onset'
  if ('time' %in% colnames(edge.data)) t2 = 'time' else t2 = 'onset'
  if (length(vertex.data[,t1]) + length(edge.data[,t2])==0) return(-Inf)
  return(min(vertex.data[,t1], edge.data[,t2]))
}

maxtime = function(vertex.data, edge.data) {
  if ('time' %in% colnames(vertex.data)) t1 = 'time' else t1 = 'terminus'
  if ('time' %in% colnames(edge.data)) t2 = 'time' else t2 = 'terminus'
  if (length(vertex.data[,t1]) + length(edge.data[,t2])==0) return(Inf)
  return(max(vertex.data[,t1], edge.data[,t2]))
}