#test networkDynamic conversion functionality
#AFTER IT INITALY PASSES TESTS, ALL 'warning' should be switched to 'stop'


require(networkDynamic)



# --------------- networkDynamic() conversion test

# vertex spells
vert.spls <-matrix(
  c(1,2,1,
  2,3,2,
  3,4,3,
  4,5,4),ncol=3,byrow=TRUE)

nd <-networkDynamic(vertex.spells=vert.spls)
if (network.size(nd)!=4){
  stop("networkDynamic did not create network of appropriate size from vertex.spells")
}
if(!all(get.vertex.activity(nd,as.spellList=TRUE)==vert.spls)){
  stop("networkDynamic did not create network with appropriate vertex.spells from vertex.spells")
}
   
# vertex.spells  impute missing vertex?   
vert.spls <-matrix(
     c(1,2,1,
       2,3,2,
       3,4,3,
       4,5,9),ncol=3,byrow=TRUE)
nd <-networkDynamic(vertex.spells=vert.spls)
if (network.size(nd)!=9){
  stop("networkDynamic did not create network of appropriate imputed size from vertex.spells")
}
   
# vertex.spells  bad spell input
vert.spls <-matrix(
     c(1,2,1,
       5,3,2,  #uh oh, this spell is backwards
       3,4,3,
       4,5,4),ncol=3,byrow=TRUE)
try(nd <-networkDynamic(vertex.spells=vert.spls)) #throws error, but doesn't say what line
   
   
# edge spells
edge.spls <-matrix(
  c(1,2,1,2,
    2,3,2,3,
    3,4,3,4,
    4,5,4,5),ncol=4,byrow=TRUE)
nd <-networkDynamic(edge.spells=edge.spls)
if(network.edgecount(nd)!=4){
  stop("networkDynamic() did not create appropriate number of edges from edge.spells")
}
if(network.size(nd)!=5){
  stop("networkDynamic() did not create appropriate number of vertices from edge.spells")
}

# edge spells - 0 edges
net<-network.initialize(5)  #will crash if no vertices
edge.spls <-matrix(ncol=4,nrow=0,byrow=TRUE)
nd <-networkDynamic(base.net=net,edge.spells=edge.spls)

# edge spells - Loops
edge.spls <-matrix(
  c(1,2,1,1,
    1,2,2,2),ncol=4,byrow=TRUE)
nd <-networkDynamic(edge.spells=edge.spls)


# NA values  # this produces not informative error
#edge.spls <-matrix(
#  c(1,2,1,NA,
#    1,2,"a",2),ncol=4,byrow=TRUE)
#nd <-networkDynamic(edge.spells=edge.spls)

# edge spells - infer network size
edge.spls <-matrix(
  c(1,2,1,1,
    1,2,2,9),ncol=4,byrow=TRUE)
nd <-networkDynamic(edge.spells=edge.spls)
if(network.size(nd)!=9){
  stop("networkDynamic() did not infer network size from edge ids as expected")
}

# network properties from base net preserved
net <-network.initialize(5,directed=FALSE,loops=TRUE)
edge.spls <-matrix(
  c(1,2,1,2,
    2,3,2,3,
    3,4,3,4,
    4,5,4,5),ncol=4,byrow=TRUE)
nd <-networkDynamic(base.net=net,edge.spells=edge.spls)
if (is.directed(nd) & !has.loops(nd)){
  stop("networkDynamic did not preserve basic network settings from base.net argument")
}

# constsruct with both edges and vertices
edge.spls <-matrix(
  c(1,2,1,2,
    2,3,2,3,
    3,4,3,4,
    4,5,4,5),ncol=4,byrow=TRUE)
vert.spls <-matrix(
  c(1,2,1,
    2,3,2,
    3,4,3,
    4,5,4),ncol=3,byrow=TRUE)
nd<-networkDynamic(vertex.spells=vert.spls, edge.spells=edge.spls)

# inconsistant edges and vertices


# vertex toggles - no base net
vrt.tog <-matrix(
  c(1,1,
    2,2,
    3,2),ncol=2,byrow=TRUE)
nd <-networkDynamic(vertex.toggles=vrt.tog)
if(!all(get.vertex.activity(nd,as.spellList=TRUE)[,1:3]==matrix(c(-Inf,1,1,-Inf,2,2,3,Inf,2),ncol=3,byrow=TRUE))){
  stop("networkDynamic() did not produduce expected output for vertex.toggles ")
}

# vertex toggles - with base net
net <-network.initialize(3)
vrt.tog <-matrix(
  c(1,1,
    2,2,
    3,2),ncol=2,byrow=TRUE)
nd <-networkDynamic(base.net=net,vertex.toggles=vrt.tog)
if(!all(get.vertex.activity(nd,as.spellList=TRUE)[,1:3]==matrix(c(-Inf,1,1,
                                                                  -Inf,2,2,
                                                                  -Inf,Inf,3,
                                                                  3,Inf,2),ncol=3,byrow=TRUE))){
  stop("networkDynamic() did not produce expected output for vertex.toggles and base.net")
}

# edge toggles
edge.tog <- matrix(
  c(1,1,2,
    2,2,3,
    3,2,3),ncol=3,byrow=TRUE)
nd<-networkDynamic(edge.toggles=edge.tog)
if(!all(get.edge.activity(nd,as.spellList=TRUE)[,1:4]==matrix(c(1,Inf,1,2,2,3,2,3),ncol=4,byrow=TRUE))){
  stop("networkDynamic() did not produce expected output for edge.toggles")
}

net<-network.initialize(4)
net[3,4]<-1
net[1,4]<-1
edge.tog <- matrix(
  c(1,1,2,
    2,2,3,
    3,2,3,
    0,1,4),ncol=3,byrow=TRUE)
nd<-networkDynamic(base.net=net,edge.toggles=edge.tog)
if(!all(get.edge.activity(nd,as.spellList=TRUE)[,1:4]==matrix(c(-Inf,Inf,3,4,-Inf,0,1,4,1,Inf,1,2,2,3,2,3),ncol=4,byrow=TRUE))){
  stop("networkDynamic() did not produce expected output for edge.toggles with base.net")
}

# vertex changes
vrt.cng <-matrix(
  c(1,1,1,
    2,2,1,
    3,2,0),ncol=3,byrow=TRUE)
nd <-networkDynamic(vertex.changes=vrt.cng)
if (!all(get.vertex.activity(nd,as.spellList=TRUE)[,1:3]==matrix(c(1,Inf,1,2, 2,3,2,3),ncol=4,byrow=TRUE) )){
  stop("networkDynamic() did not produce expected output for vertex.changes argument")
}

# edge changes
edge.cng <-matrix(
  c(1,1,2,1,
    2,2,3,1,
    3,2,3,0),ncol=4,byrow=TRUE)
nd <-networkDynamic(edge.changes=edge.cng)
if (!all(get.edge.activity(nd,as.spellList=TRUE)[,1:4]==matrix(c(1,Inf,1,2, 2,3,2,3),ncol=4,byrow=TRUE))){
  stop("networkDynamic() did not produce expected output for edge.changes argument")
}

# edge changes - activate edge allready active ignored
edge.cng <-matrix(
  c(2,2,3,1,
    3,2,3,1),ncol=4,byrow=TRUE)
nd <-networkDynamic(edge.changes=edge.cng)

# check net.obs.period defaults

# ===============
# testing as.networkDynaic.network.list 
# 
#try converting the newcomb panel data (working 9/3)
data(newcomb)
newDyn <- networkDynamic(network.list=newcomb)
#does it pass a consistency check?
check <- network.dynamic.check(newDyn) 
if (!all(sapply(check, all)))
  stop("newcomb network.list conversion did not pass network.dynamic.check")
#is the matrix equal to the input matrix
for (k in 1:length(newcomb)) {
  if (!all(as.sociomatrix(newcomb[[k]]) == as.sociomatrix(network.extract(newDyn,onset=k-1,terminus=k)))){
    stop("FAIL: ndConverter: 1st input matrix does not match crosssection from time 0 to 1 for newcomb example")
  }
}

# try converting a list that includes different size networks. (working 9/10)
# note that beach[[25]] is NA (missing)
data(windsurferPanels)
beach<-beach[-25]
# should return error
dynBeach=NULL
dynBeach<-tryCatch(dynBeach<-networkDynamic(network.list = beach), error=function(e){TRUE})
if (is.network(dynBeach) || dynBeach!=TRUE) stop("did not ask for vertex.pid when network.list have different size networks")

dynBeach<-networkDynamic(network.list=beach, vertex.pid="vertex.names")

#data level node indicies are stored as the vertex names

#check if the neighborhood is the same for both.
for (i in 1:length(beach)) {
  if (!identical(beach[[i]], NA)) {
    for (j in network.vertex.names(beach[[i]])) {
      ng1 <-get.neighborhood(beach[[i]], v=networkDynamic:::get.vertex.id(beach[[i]], j))
      ng2 <- get.neighborhood.active(dynBeach, onset=i-1, terminus=i, v=networkDynamic:::get.vertex.id(dynBeach, j))
      # the following line is much much slower
      #ng2 <-get.neighborhood(network.extract(dynBeach,onset=i-1,terminus=i,retain.all.vertices=T),v=get.vertex.id(dynBeach, j))
      # need to check the vertex names, not the ids which are changed when converting to a networkDynamic object
      names1 <- sort(network.vertex.names(beach[[i]])[ng1])
      names2 <- sort(network.vertex.names(dynBeach)[ng2])
      # print these if necessary
      # print(paste('============ ', i, '=========='))
      # print(names1)
      # print(names2)
      if (!identical(names1, names2)) {
        print(paste("FAIL: networkDynamic(): neigborhoods do not match for variable sized network list example (windsurfers)",
                   " at time", i, 'vertex', j))
        print(names1)
        print(names2)
      }
    }
    
  }
}

# try a better representation that preserves edge times

dynBeach<-networkDynamic(network.list=beach, vertex.pid="vertex.names",onsets=c(1:24,26:31),termini=c(2:25,27:32))

# make sure day 25 really is missing
if (any(is.active(dynBeach, at=25,v=1:network.size(dynBeach)))){
  stop("onsets and termini did not correctly omit day 25 in windsurfer example")
}


#try the version that has edge weights
#newRankDyn <-networkDynamic(newcomb.rank)



# check correct spells printed for edges
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,onset=1,terminus=2)
ref <- matrix(c(1,2,1,2,0,0,1,1, 1,2,2,3,0,0,1,2),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for edge spells from as.networkDynamic.data.frame") 
}

test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=3)
ref <- matrix(c(3,3,1,2,0,0,0,1, 3,3,2,3,0,0,0,2),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for edge spells of zero length from as.networkDynamic.data.frame") 
}

# test for missing activity attribute (only set on one edge)
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=3,e=1)
as.data.frame(test)
tryCatch(
  as.data.frame(test), error = function(e){ warning(paste("error in as.networkDynamic.data.frame  for edge with missing activity attribute",e))} )


#check for duration for funny length spells
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=1,e=1)
activate.edges(test,onset=2.7,terminus=5, e=2)
ref <- matrix(c(1.0,1,1,2,0,0,0,1, 2.7,5,2,3,0,0,2.3,2),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for edge spells from as.networkDynamic.data.frame") 
}

# check multiple spells per edge
test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=1,terminus=2)
activate.edges(test,onset=3,terminus=4)
ref <- matrix(c(1,2,1,2,0,0,1,1, 3,4,1,2,0,0,1,1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  stop("unexpected output for multiple spells per edge for as.networkDynamic.data.frame") 
}

# check censoring arguments when passed in
test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=-Inf,terminus=10)
ref <- matrix(c(5,10,1,2,1,0,5,1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test,start=5) == ref)){
  stop("unexpected output for 'start' left censoring argument from as.networkDynamic.data.frame") 
}

test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=0,terminus=Inf)
ref <- matrix(c(0,5,1,2,0,1,5,1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test,end=5) == ref)){
  stop("unexpected output for 'end' left censoring argument from as.networkDynamic.data.frame") 
}

test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=-Inf,terminus=Inf)
ref <- matrix(c(-Inf,Inf,1,2,1,1,Inf, 1),ncol=8,byrow=TRUE)
if (!all(as.data.frame(test) == ref)){
  warning("unexpected output for as.networkDynamic.data.frame: Inf and -Inf times not treated as censored") 
}

# check censoring arguments when set on input object using attr
test <- network.initialize(3)
test[1,2]<-1
activate.edges(test,onset=-Inf,terminus=Inf)
attr(test,"start")<-5
as.data.frame(test)
#skye: this seems to work, but I want to remove the feature in favor of net.obs.period



# =============== TESTING networkDynamic ===========================

# working 9/3/2012. Just compare the data frame output with edgetimes
# a really crude edgelist example
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 1,2,3,4,  2,3,1,3 ),ncol=4,byrow=TRUE))
edgetimetest<-networkDynamic(edge.spells = edgetimes)
# do the edges and spells match when spit back out?
if (!all(as.data.frame(edgetimetest)[,1:4]==edgetimes)){
  stop("output spell matrix does not match input for networkDynamic()")
}

#does the internal representation match?
if( !all(as.vector(edgetimetest$mel[[1]]$atl$active) == c(1,2))){
  stop("networkDynamic() gave unexpected internal representation spells")
}


# combining multiple spells (should combine the spells for edge between v1 and v2)
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,3,1,2,  1,2,3,4 ),ncol=4,byrow=TRUE))
edgetimetest<-networkDynamic(edge.spells = edgetimes)
if (!all(as.data.frame(edgetimetest)[,1:4]==matrix(c(1,3,1,2, 1,2,3,4),ncol=4,byrow=TRUE))){
  stop("output spell matrix did not merge input spells as expected for networkDynamic()")
}


# with censoring
# Skye:  why does input of Inf mean that it is right censored?
edgetimes <- as.data.frame(matrix( c(1,Inf,1,2, 2,3,2,3),ncol=4,byrow=TRUE))
edgetimetest<-networkDynamic(edge.spells = edgetimes)
if (!all(as.data.frame(edgetimetest)[,1:4]==edgetimes)){
  stop("output spell matrix did not merge input spells as expected for networkDynamic()")
}

# with missing node (should fill in the missing node)
edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,4,1,2,  1,2,1,4 ),ncol=4,byrow=TRUE))
edgetimetest<-networkDynamic(edge.spells = edgetimes)
if (network.size(edgetimetest)!=4){
  stop("networkDynamic() did not create network with implied size of 4")
}


# create with vertex dynamics specified
# nodeInfo: should be a dataframe of the format
#   vertex.id   onset   terminus   NodeId(aka vertex name)
# nodetimes <-as.data.frame(matrix( c(1,1,2,1, 2,2,3,2,  3,3,4,3 ),ncol=4,byrow=TRUE))
# edgetimes <- as.data.frame(matrix( c(1,2,1,2, 2,4,1,2,  1,2,1,4 ),ncol=4,byrow=TRUE))
# as.networkDynamic(edgetimes,nodeInfo=nodetimes)

# networkDynamic network.list conversion

# does it create a dynamic network
d1 <- network.initialize(3)
d1[1,2]<-1
d2 <- network.initialize(3)
d2[1,2]<-1
d2[2,3]<-1
d3 <- network.initialize(3)
d3[3,1]<-1

# default timing
ddyn <- networkDynamic(network.list = list(d1,d2,d3))

if(!is.networkDynamic(ddyn)){
  stop("as.networkDynamic.list didn't create a dynamic network from ")
}

# check that correct spells with unit lengths were created
ref <- matrix(c(0,2,1,2,0,0,2, 1,2,2,3,0,0,1, 2,3,3,1,0,0,1),ncol=7,byrow=TRUE)
if (!all(as.data.frame(ddyn)[,1:7]==ref)){
  stop("correct unit length spells were not created for list input networks in as.networkDynamic.list")
}

# does it preserve network attributes of passed in network
d1 <- network.initialize(2,directed=F,bipartite=T,multiple=T,loops=T)
d2 <- network.initialize(2,directed=F,bipartite=T,multiple=T,loops=T)
dlist <- list(d1,d2)
ddyn <- networkDynamic(network.list = dlist)

if (is.directed(ddyn) != FALSE){
  stop("'directed' argument if initial network in list not respected in dynamic version")
}
if (is.bipartite(ddyn) != TRUE){
  stop("'bipartite' argument if initial network in list not respected in dynamic version")
}
if (is.multiplex(ddyn) != TRUE){
  stop("'multiple' argument if initial network in list not respected in dynamic version")
}
if (has.loops(ddyn) != TRUE){
  stop("'loops' argument if initial network in list not respected in dynamic version")
}


# does it warn if network attributes of passed in networks do not match
# working 9/10
d1 <- network.initialize(2,directed=F)
d2 <- network.initialize(2,directed=T)
dlist <- list(d1,d2)
if (tryCatch(networkDynamic(network.list = dlist), warning=function(w) return(F)))
  stop("different network attributes in network.list did not result in a warning")

# specify net.obs.period
#nop = list(observations=list(c(3,5)), mode="discrete", time.increment=1,time.unit="step")
#ddyn <- networkDynamic(network.list = list(d1,d2,d3), net.obs.period=nop)
#as.data.frame(ddyn)


# =============== TESTING as.data.frame.networkDynamic ====

#lets try passing in a noncensored and duration, see if we get the same things back
edgetimes <- as.data.frame(matrix( c(1,2,1,2,0,0, 3,5,1,2,0,0,1,2,3,4,0,0, 3,4,2,4,0,0 ),ncol=6,byrow=TRUE))
colnames(edgetimes)<-c("onset","terminus","tail","head","onset.censored","terminus.censored")
testnet <- network.initialize(4)
add.edges.active(testnet,onset=1,terminus=2,tail=1,head=2)
add.edges.active(testnet,onset=1,terminus=2,tail=3,head=4)
activate.edges(testnet,onset=3,terminus=5,e=get.edgeIDs(testnet, v=1,alter=2))
add.edges.active(testnet,onset=3,terminus=4,tail=2,head=4)
# these should match
if (!all(as.data.frame(testnet)[,1:6] == edgetimes)){
  stop("FAIL: output data.frame from as.data.frame.networkDynamic did not match input")
}

#check column names
if(!all(names(as.data.frame(testnet))==c("onset","terminus","tail","head","onset.censored","terminus.censored","duration","edge.id"))){
  stop("Unexpected column names returned by as.data.frame.networkDynamic")
  
}

# censoring should set the appropriate start or end to Inf
# skye: Is this the behavior we want for as.data.frame?
edgetimes <- as.data.frame(matrix( c(0,2,1,2,1,0,2, 3,5,1,2,0,0,2,  1,2,3,4,0,0,1, 3,6,2,4,0,1,3 ),ncol=7,byrow=TRUE))
colnames(edgetimes)<-c("onset","terminus","tail","head","onset.censored","terminus.censored","duration")
testnet <- network.initialize(4)
add.edges.active(testnet,onset=-Inf,terminus=2,tail=1,head=2)
add.edges.active(testnet,onset=1,terminus=2,tail=3,head=4)
activate.edges(testnet,onset=3,terminus=5,e=get.edgeIDs(testnet, v=1,alter=2))
add.edges.active(testnet,onset=3,terminus=Inf,tail=2,head=4)

# these should match
if(!all(as.data.frame(testnet,start=0,end=6)[-8]==edgetimes)){
  stop("as.data.frame.networkDynamic gave unexpected censored spell matrix output")
}


# properly handle edges with no spell activity
test <- network.initialize(3)
test[1,2]<-1
test[2,3]<-1
activate.edges(test,at=3,e=1)
#tryCatch(temp = as.data.frame(test), error = function(e) stop("as.networkDynamic.data.frame() did not handle edges with no spell activity"))
temp = as.data.frame(test)
if (!all(temp == c(3,3,1,2,F,F,0,1))) stop('did not handle edges without spell activity')

# properly handle nD object with no edge spell activity at all
net <-network.initialize(3)
net[1,2]<-1;
net[2,3]<-1;
activate.vertices(net, onset=1, terminus=Inf)
temp = as.data.frame(net)
if (nrow(temp) != 0) {
  stop("as.data.frame.networkDynamic() did not handle an object without any edge activity")
}

# test networkDynamic (function used by TERGM)

test <- network.initialize(3)
test[1,3]<-1
tog <- matrix(c(1,1,2, 1,2,3, 2,1,2, 4,1,3, 4,1,2), ncol=3, byrow=TRUE)
net<-networkDynamic(base.net=test,edge.toggles=tog)
spells <-as.data.frame(net)[,1:4]
# first spell should start at -Inf because edge was in original net
# all edges in original net are considered active before toggles
if (!all(spells[1,]==c(-Inf,4,1,3))){
  stop("networkDynamic() did not record initial toggle correctly")
}
# 2nd spell toggles twice
if (!all(spells[2,]==c(1,2,1,2))){
  stop("networkDynamic() did not record double toggle correctly")
}
if (!all(spells[4,]==c(1,Inf,2,3)) | !all(spells[3,]==c(4,Inf,1,2))){
  stop("networkDynamic() did not record toggles correctly")
}



# ==================== TESTING duration.matrix
# this function is used internally by as.networkDynamic.network() and should not be called by user
net <-network.initialize(3)
net[1,2]<-1;
net[2,3]<-1;
net[1,3]<-1;
# toggle list: time, tail, head
tog<-matrix(c(1,1,2, 1,2,3, 2,1,2, 4,1,3, 4,1,2), ncol=3, byrow=TRUE)
# we expect this matrix
ref <- matrix(c(0,1,1,2,1,0,1, 0,1,2,3,1,0,1, 0,4,1,3,1,0,4, 2,4,1,2,0,0,2),ncol=7,byrow=TRUE)
if (!all(networkDynamic:::duration.matrix(net, changes=tog, start=0, end=5)==ref)){
  stop("duration.matrix returned an unexpected spell list for its input toggles")
}
# testing start and end
ref1 <- matrix(c(1,1,1,2,1,0,0, 1,1,2,3,1,0,0, 1,4,1,3,1,0,3, 2,4,1,2,0,0,2),ncol=7,byrow=TRUE)
if (!all(networkDynamic:::duration.matrix(net, changes=tog, start=1, end=5)==ref1)){
  stop("duration.matrix returned an unexpected spell list for its input toggles")
}
# testing start and end
ref2 <- matrix(c(0,1,1,2,1,0,1, 0,1,2,3,1,0,1, 0,4,1,3,1,0,4, 2,4,1,2,0,0,2),ncol=7,byrow=TRUE)
if (!all(networkDynamic:::duration.matrix(net, changes=tog, start=0, end=8)==ref2)){
  stop("duration.matrix returned an unexpected spell list for its input toggles")
}


