#  File networkDynamic/R/extract.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
# Contents:
#
# "%t%.network"
# network.extract
# network.dynamic.check
#
######################################################################

#Operator form for network.extract
"%t%"<-function(x,at){
  network.extract(x=x,at=at)
}


#Function to take a temporal extract/cross-section of a dynamically extended
#network object.  Not very long-term safe....
#retain.all.verticies added if you want to not remove inactive verticies so networks stay the same size for comparison
network.extract<-function(x,onset=NULL,terminus=NULL,length=NULL, at=NULL,
                               rule=c("any","all"),active.default=TRUE,retain.all.vertices=FALSE,trim.spells=FALSE){
  # determine which nodes/edges are active
  # nodes activity is straight forward
  # edge activity depends on the activity of the edge, but
  # also the activity of it's in/out (tail/head) nodes
  activeV<-is.active(x=x,onset=onset,terminus=terminus,length=length,at=at,
                     e=NULL,v=seq_len(x%n%"n"), rule=rule, active.default=active.default)
  activeE=logical(0)
  if(length(x$mel)){
    activeE<-is.active(x=x,onset=onset,terminus=terminus,length=length,at=at,
                       e=seq_along(x$mel),v=NULL, rule=rule, active.default=active.default)
    nullE <- sapply(x$mel, is.null)
    inV = sapply(x$mel[!nullE], "[[", "inl")  # in nodes of edges
    outV = sapply(x$mel[!nullE], "[[", "outl")  # out nodes of edges
    activeTH = sapply(1:length(inV), function(x){activeV[inV[x]] && activeV[outV[x]]})  # are head and tail active?
  }
  if(retain.all.vertices)
    newVid = seq(x%n%"n")    # node id mapping
  else
    newVid = cumsum(activeV)
  
  # Create network
  n<-ifelse(retain.all.vertices, x%n%"n", sum(activeV))
  if(n==0)
    return(list())
  net<-network.initialize(n)
  # Set network-level attributes
  net$gal<-as.list(x$gal)
  net%n%"n"<-n
  net%n%"mnext"<-1
  if(net%n%"bipartite">0)
    net%n%"bipartite"<-newVid[net%n%"bipartite"]
  # Set vertex-level attributes
  if(n>0)
    net$val<-as.list(x$val[activeV])
  # Add edges
  if(length(activeE)){
    activeETH = activeE & activeTH
    if(any(activeETH)){
      tail<-as.list(lapply(x$mel[!nullE][activeETH],function(z){newVid[z$outl]}))
      head<-as.list(lapply(x$mel[!nullE][activeETH],function(z){newVid[z$inl]}))
      atl<-as.list(lapply(x$mel[!nullE][activeETH],"[[","atl"))
      nam<-as.list(lapply(x$mel[!nullE][activeETH],function(z){names(z$atl)}))
      add.edges(net,tail=tail,head=head,names.eval=nam,vals.eval=atl)
    }
  }
  if (trim.spells){
    # delete extra spell data on vertices edges and attributes that would be outside the query time range.
    # figure out onset and terminus from at and length if necessary
    if(!is.null(at)) {
      onset <- terminus <- at
    } else if (!is.null(onset)) {
      
      if (!is.null(length))
        terminus <- onset + length
    } else {
      if (is.null(terminus)) {
        onset <- -Inf
        terminus <- Inf
      } else {
        onset <- terminus - length
      }
    }
    deactivate.vertices(net,onset=-Inf,terminus=onset)
    deactivate.vertices(net,onset=terminus,terminus=Inf)
    deactivate.edges(net,onset=-Inf,terminus=onset)
    deactivate.edges(net,onset=terminus,terminus=Inf)
    active.edge.attrs <-gsub(".active","",grep(".active",list.edge.attributes(net),value=TRUE))
    if(length(active.edge.attrs)>0){
      for(attr in active.edge.attrs){
        deactivate.edge.attribute(net,attr,onset=-Inf,terminus=onset)
        deactivate.edge.attribute(net,attr,onset=terminus,terminus=Inf)
      }
    }
    active.vertex.attrs <-gsub(".active","",grep(".active",list.vertex.attributes(net),value=TRUE))
    if(length (active.vertex.attrs)>0){
      for(attr in active.vertex.attrs){
        deactivate.vertex.attribute(net,attr,onset=-Inf,terminus=onset)
        deactivate.vertex.attribute(net,attr,onset=terminus,terminus=Inf)
      }
    }
    active.net.attrs <-gsub(".active","",grep(".active",list.network.attributes(net),value=TRUE))
    if(length(active.net.attrs)>0){
      for(attr in active.net.attrs){
        deactivate.network.attribute(net,attr,onset=-Inf,terminus=onset)
        deactivate.network.attribute(net,attr,onset=terminus,terminus=Inf)
      }
    }
  }
  # todo: update net.obs.period censoring info
  set.nD.class(net)

}

# internal Check spell matrix function to veryfiy consistency of spell matricies
# returns a logical vector where first element says if dimensions are ok, second if spells are ok
chkspellmat<-function(z){
  goodDims=TRUE
  goodSpells=TRUE
  if(!is.null(z)) {
    if(length(dim(z))!=2) {
      goodDims=FALSE
    } else {
      if(dim(z)[2]!=2) {
        goodDims=FALSE
      } else {
        if(NROW(z)==1){ #Single spell - either equal, ordered or Inf,Inf
          if(all(z==Inf)||(z[1,2]>=z[1,1]))
            goodSpells=TRUE
          else
            goodSpells=FALSE
        }else{       #Multiple spells - equal, ordered, non-overlapping
          if(all(z[,2]>=z[,1])&&all(z[-1,1]-z[-NROW(z),2]>=0))
            goodSpells=TRUE
          else
            goodSpells=FALSE
        }
      }
    }
  }
  c(goodDims, goodSpells)
} # end spell mat function

# checks that a tea attribute has appropriate structure, returning true if ok
chkTeaAttrOk <- function(x){
  if(!is.list(x)){
    return(FALSE)
  }
  if (length(x)==1 && is.na(x[[1]])){
    return(TRUE) # its not bad, its just not there
  }
  if(length(x)!=2){
    return(FALSE)
  }
  if(!is.list(x[[1]])){
    return(FALSE)
  }
  if(!is.matrix(x[[2]])){
    return(FALSE)
  }
  if(length(x[[1]])!=nrow(x[[2]])){
    return(FALSE)
  }
  if(!all(chkspellmat(x[[2]]))){
    return(FALSE)
  }  
  
  return(TRUE)
}

#Function to check dynamic consistency of network objects.  Not terribly safe,
#long-term.
network.dynamic.check<-function(x,verbose=TRUE, complete=TRUE){
  if(!is.network(x))
    stop("network.dynamic.check requires an object of class network.\n")
  pass.complete=TRUE
  if (complete) {
    
    #Check to ensure that vertex activity matrices are legit
    vertok<-sapply(x$val,function(y){chkspellmat(y$active)})
    if(verbose && any(!vertok[1,])) {
      cat("The dimensionality of the spell matrices is incorrect for vertex/vertices ")
      cat(which(!vertok[1,]))
      cat(".\n")
    }
    if(verbose && any(!vertok[2,])) {
      cat("The ordering of the spell matrices is incorrect for vertex/vertices ")
      cat(which(!vertok[2,]))
      cat(".\n")
    }
    vertok = apply(vertok, 2, function(x){x[1] & x[2]})
    if(any(!vertok)) pass.complete=FALSE
    
    # check vertex attribute activity
    vrtAttrOk <- rep(TRUE,length=network.size(x))
    dynVrtAttrs <- grep(".active",list.vertex.attributes(x),value=TRUE)
    for (attr in dynVrtAttrs){
      badVrts <- which(!sapply(get.vertex.attribute(x,attr,unlist=FALSE),chkTeaAttrOk))
      if (length(badVrts)>0){
        vrtAttrOk[badVrts] <-FALSE;
        if (verbose){
          cat(paste("Dynamic vertex attribute '",attr,"' is malformed for vertex ids",paste(badVrts,collapse=" "),"\n"))
        }
      }
    }
    
    if(network.edgecount(x)>0){
      #Check to ensure that edge activity matrices are OK
      active <- lapply(lapply(x$mel, "[[", "atl"), "[[", "active")
      edgeok <- sapply(active, chkspellmat)
      if(verbose && any(!edgeok[1,])) {
        cat("The dimensionality of the spell matrices is incorrect for edge(s) ")
        cat(which(!edgeok[1,]))
        cat(".\n")
      }
      if(verbose && any(!edgeok[2,])) {
        cat("The ordering of the spell matrices is incorrect for edge(s) ")
        cat(which(!edgeok[2,]))
        cat(".\n")
      }
      edgeok = apply(edgeok, 2, function(x){x[1] & x[2]})
      if(any(!edgeok)) pass.complete=FALSE
      
      # check edge attribute activity
      edgeAttrOk <- rep(TRUE,length=network.edgecount(x))
      dynEdgeAttrs <- grep(".active",list.edge.attributes(x),value=TRUE)
      for (attr in dynEdgeAttrs){
        badE <- which(!sapply(get.edge.value(x,attr,unlist=FALSE),chkTeaAttrOk))
        if (length(badE)>0){
          edgeAttrOk[badE] <-FALSE;
          if (verbose){
            cat(paste("Dynamic edge attribute '",attr,"' is malformed for edge ids",paste(badE,collapse=" "),"\n"))
          }
        }
      }
    } else {
      edgeok <-logical(0)
      edgeAttrOk <- logical(0)
    }
    
    # check network attribute activity
    netAttrOk <- TRUE
    dynNetAttrs <- grep(".active",list.network.attributes(x),value=TRUE)
    for (attr in dynNetAttrs){
      badN <- chkTeaAttrOk(get.network.attribute(x,attr,unlist=FALSE))
      if (!badN){
        netAttrOk <-FALSE;
        if (verbose){
          cat(paste("Dynamic network attribute '",attr,"' is malformed\n"))
        }
      }
    }
  } # end complete block

  if(pass.complete) {  
    dyadok<-rep(F,length(x$mel))
    for(i in seq_along(x$mel)) {
      if(is.null(x$mel[[i]])) {
        dyadok[i]<-TRUE
      } else {
        y<-x$mel[[i]]
        ep<-c(y$outl,y$inl)
        act<-y$atl$active
        if (is.null(act)) {
          dyadok[i] <- TRUE
        } else {
          #Verify endpoint activity
          flag<-TRUE
          for(j in seq_len(nrow(act)))          #Check each spell...
            if(any(act[j,]!=c(Inf,Inf)))        #Not a placeholder spell...
              for(k in seq_along(ep))           #...against each endpoint
                if(flag) {
                  active.try = try(flag <- is.active(x=x,onset=act[j,1],terminus=act[j,2],v=ep[k],
                                   rule="all"), silent=T)
                  if(class(active.try)=="try-error") {
                    flag <- FALSE
                    warn.text="This check encountered an error in the is.active function."
                    if(!complete) warn.text=paste(warn.text, "Re-run check with 'complete=T'")
                    warning(warn.text)
                  } else {
                    flag <- active.try
                  }
                }
          dyadok[i]<-flag
        }
      }
    }
    if(verbose && any(!dyadok)){
      cat("Edges were found active where the endpoints where not in edge(s) ")
      cat(which(!dyadok))
      cat(".\n")
    }
  }

  #Return the results
  if(complete && pass.complete) {
    list(vertex.checks=vertok, edge.checks=edgeok, dyad.checks=dyadok,vertex.tea.checks=vrtAttrOk,edge.tea.checks=edgeAttrOk,network.tea.checks=netAttrOk)
  } else if (complete) {
    list(vertex.checks=vertok, edge.checks=edgeok)
  } else {
    list(dyad.checks=dyadok)
  }
}

