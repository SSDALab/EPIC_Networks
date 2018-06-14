# tests to determine if the networkDynamic class is being set appropriately tested for

library(networkDynamic)
#check if activate.edge sets class.
net <- network.initialize(5)
net[1,2] <-1
nD <-activate.edges(net,onset=0,terminus=1)
if (!is.networkDynamic(nD)){
  stop("activate edges did not set networkDynamic class")
}
nd <- NULL

net <- network.initialize(5)
net[1,2] <-1
nD <-activate.vertices(net,onset=0,terminus=1)
if (!is.networkDynamic(nD)){
  stop("activate vertices did not set networkDynamic class")
}
