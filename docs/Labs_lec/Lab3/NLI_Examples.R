#######################################
###
### NLIs: Some Examples
###
#######################################

#######################################
###
### function that takes a list of network objects and outputs classic
###    centrality scores by nodes and ranks them
#######################################
node.centrality<-function(list.net){
  lapply(list.net,
         function(z){
           x<-symmetrize(as.sociomatrix.sna(z))
           central.nodes<-cbind(degree(x,cmode="indegree"), degree(x,cmode="outdegree"),evcent(x),betweenness(x,rescale=TRUE),
                                closeness(x,cmode="suminvundir"))
           colnames(central.nodes)<-c("idegree","odegree","eigen","betweenness","closeness")
           rownames(central.nodes)<-attr(z,"vnames")
           
           o1<-order(central.nodes[,1],decreasing =TRUE)
           o2<-order(central.nodes[,2],decreasing =TRUE)
           o3<-order(central.nodes[,3],decreasing =TRUE)
           o4<-order(central.nodes[,4],decreasing =TRUE)
           o5<-order(central.nodes[,5],decreasing =TRUE)
           list(ranking=central.nodes,order=cbind(o1,o2,o3,o4,o5))
         })
}

#########################
### Let's do an example
#########################

### Lin Freeman's famous Beach data
#help(beach)
data(beach)


### Compute Centrality scores
nc<-node.centrality(beach)

### Print
lapply(nc,function(x){head(x[[1]])})

### Print ordered by degre
lapply(nc,function(x){head(x[[1]][x$order[,1],])})

################
## Class exercise, perform some NLI analysis!
################


#######################################
###
### GLIs: Some Examples
###
#######################################

##Centralization follows Freeman's (1979) generalized definition of network centralization, and can be used with any properly defined centrality measure. This measure must be implemented separately; see the references below for examples.
### The centralization of a graph G for centrality measure C(v) is 
### defined (as per Freeman (1979)) to be:
###C^*(G) = sum( |max(C(v))-C(i)|, i in V(G) )


help(centralization)
centralization(beach,g=1,degree,cmode="indegree")
centralization(beach,g=2,degree,cmode="indegree")







