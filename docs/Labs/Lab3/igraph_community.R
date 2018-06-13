##########################################################################
#	R & igraph Tutorial for EPIC SNA 2018
#	author: jimi adams
# Modified: Zack W. Almquist
#	last updated: 2017-06-03
# modified: 2018-06-11
#
##########################################################################

##########################################################################
#	0. 	Setting up your R environment
##########################################################################
#setwd("FILEPATH")	# Path for all unspecified session files
#install.packages("igraph")				# The first time you install a package
library(igraph)							# Attach the library to your session
#update.packages("igraph")				# Gets the most recent package version
# check might need data/data if windows
load("data/Tutorial.Rdata")
ls()									# What's in the data file?

##########################################################################
#	5. 		Centralities 
##########################################################################
library(igraph)
class(cent_ex1a)
class(c_ex1a)
# w/ igraph
ideg <- igraph::degree(cent_ex1a)
iclose <- igraph::closeness(cent_ex1a, normalized=T)
ibtwn <- igraph::betweenness(cent_ex1a)

# w/ sna
detach(package:igraph)	#to avoid conflicts
sdeg <- sna::degree(c_ex1a, gmode="graph")			
sclose <- sna::closeness(c_ex1a, gmode="graph")		
sbtwn <- sna::betweenness(c_ex1a, gmode="graph")	
# putting together for comparison
ex_1a <- data.frame(ideg=ideg, sdeg=sdeg, iclose=iclose, sclose=sclose, ibtwn=ibtwn, sbtwn=sbtwn)
ex_1a$iclose==ex_1a$sclose
ex_1a
# standardizing the betweenness measure
nbtwn <- function(scores, size=length(scores)) {2*scores/(size*size-3*size+2)}	
nbtwn(ex_1a$sbtwn)

# modifying the example (deleting a single edge)
library(igraph)
cent_ex <- igraph::delete.edges(cent_ex1a, E(cent_ex1a, P=c("a2","a1"))) # dropping the edge between a2 & a1
ideg <- igraph::degree(cent_ex)	
iclose <- igraph::closeness(cent_ex, normalized=T)	
ibtwn <- nbtwn(igraph::betweenness(cent_ex))	
ex_df <- data.frame(ideg=ideg, iclose=iclose, ibtwn=ibtwn)

# adding the centrality information onto the plot's node size
par(mfrow=c(1,2))										# 2 plots, side-by-side
plot.igraph(cent_ex, vertex.size=3+ideg, vertex.label=NA)	# values as an additive scalar
plot.network(c_ex1a, vertex.cex=4*nbtwn(sbtwn))			# values used as multipliers
par(mfrow=c(1,1))										# returning to single plots

# using the sna package to reproduce the other examples from the slides
# it may be worthwhile to plot each of them to remind yourself what these look like
detach(package:igraph)
sdeg_b <- degree(c_ex1b, gmode="graph")
sclose_b <- closeness(c_ex1b, gmode="graph")
sbtwn_b <- nbtwn(betweenness(c_ex1b, gmode="graph"))
sdeg_2 <- degree(c_ex2, gmode="graph")
sclose_2 <- closeness(c_ex2, gmode="graph")
sbtwn_2 <- nbtwn(betweenness(c_ex2, gmode="graph"))
ex_1b <- data.frame(ID=get.vertex.attribute(c_ex1b, "vertex.names"), sdeg=sdeg_b, sclose=sclose_b, sbtwn=sbtwn_b)
ex_2 <- data.frame(sdeg=sdeg_2, sclose=sclose_2, sbtwn=sbtwn_2)

# with some real data
library(igraph)
V(korea1)$deg <- igraph::degree(korea1)
V(korea1)$close <- igraph::closeness(korea1, normalized=T)
V(korea1)$btwn <- igraph::betweenness(korea1)
#nbtwn <- function(scores, size=length(scores)) {2*scores/(size*size-3*size+2)}	
V(korea1)$nbtwn <- nbtwn(V(korea1)$btwn)
df1 <- data.frame(id=V(korea1)$id, deg=V(korea1)$deg, close=V(korea1)$close, btwn=V(korea1)$nbtwn, memb=V(korea1)$club, adopt=V(korea1)$adopt)

##########################################################################
#	6.1. 	Sub-group Cohesion 
#	6.1.1. 	@ the extremes
##########################################################################
#install.packages("intergraph")			# The first time you install a package
library(intergraph)						# Attach the library to your session
class(ex1a_g)
clusters(ex1a_g)				# similar to component.dist above
plot.igraph(ex1a_g)				# does that make sense?

ex1a_n <- asNetwork(ex1a_g)		# creating sna version
detach(package:igraph)
plot.network(ex1a_n, displaylabels=T)	# checking to be sure it's the same
isolates(ex1a_n)				# which nodes are isolates?
clique.census(ex1a_n)			# complete clique list
components(ex1a_n)				# just tells us how many there are
component.dist(ex1a_n)			# gives the distribution of component memberships & sizes
bicomponent.dist(ex1a_n)		# same as above, but >= 2 node independent paths
component.largest(ex1a_n)		# returns a logical vector of nodes' large comp membership

# Plotting just the 1st component
plot.network(ex1a_n %s% which(component.largest(ex1a_n)==TRUE), displaylabels=T)
# Another way to do the same
plot.network(ex1a_n %s% which(component.dist(ex1a_n)$membership==1), displaylabels=T)

#Plotting just the 1st bicomponent
plot(ex1a_n %s% bicomponent.dist(ex1a_n)$members$`1`, displaylabels=T)

########################################################################## 
#	6.1.2. 	Nested Cohesion
##########################################################################
library(igraph)
plot.igraph(karate)
kblocks <- cohesive.blocks(karate)		# just tells us how many there are
kblocks									# calling it provides a summary of the block structure
plot(kblocks, karate, vertex.size=6)	# take a look

##########################################################################
#	6.1.3. 	Network Communities
##########################################################################
detach(package:sna)				# to avoid conflicts
detach(package:network, force=T)

fc <- fastgreedy.community(karate)
ebc <- edge.betweenness.community(karate)
lou <- multilevel.community(karate)
comms <- data.frame(fc=fc$membership, ebc=ebc$membership, louvain=lou$membership)
par(mfrow=c(1,3))	# setting up a parallel plot 
karate$layout <- layout.kamada.kawai(karate)	# layout once so nodes don't move
karate
plot(karate, vertex.color=comms$fc)	
plot(karate, vertex.color=comms$ebc)	
plot(karate, vertex.color=comms$louvain)	
par(mfrow=c(1,1))	# returning plots to normal

V(karate)$shape <- ifelse(V(karate)$split == 1, "circle", "square")
par(mfrow=c(1,3))
plot(karate, vertex.color=comms$fc, vertex.shape=V(karate)$shape)
plot(karate, vertex.color=comms$ebc, vertex.shape=V(karate)$shape)
plot(karate, vertex.color=comms$louvain, vertex.shape=V(karate)$shape)
par(mfrow=c(1,1))

V(karate)$split[igraph::neighborhood(karate, 1,3)[[1]]]
V(karate)$split[igraph::neighborhood(karate, 1,9)[[1]]]

##########################################################################
#	7.1 	Equivalence via Blockmodels
##########################################################################
library(sna)
library(network)
detach(package:igraph)				# to avoid conflicts

plot.network(f, label=get.vertex.attribute(f, "id"))	
fc <- equiv.clust(f, mode="digraph")
fc
plot(fc)
f.bm2 <- blockmodel(f, fc, k=2)		#looking at it made the 2-solution clear
f.bm2
plot.blockmodel(f.bm2)				# take a look
f.bimage <- f.bm2$block.model		# pulling off just the block image
f.bimage
gplot(f.bimage, diag=TRUE) 			# let's plot the image network
plot.sociomatrix(f.bimage)			# the image matrix

plot.network(ex, displaylabels=T)	
exc <- equiv.clust(ex, mode="graph")
plot(exc)
ex.bm4 <- blockmodel(ex, exc, k=4)	# looking at it made the 4-solution clear
plot.blockmodel(ex.bm4)				# take a look
ex.bm4.bimage <- ex.bm4$block.model	# pulling off just the block image
gplot(ex.bm4.bimage, diag=TRUE) 	# let's plot the image network
plot.sociomatrix(ex.bm4.bimage)		# the image matrix

##########################################################################
#	7.2 	Blockmodels w/ Real Data
##########################################################################
plot.network(valente_n, displaylabels=T)	
ec <- equiv.clust(valente_n, mode="graph", plabels=network.vertex.names(valente_n)) 	# generating the clusters
#ec											# note the options
plot(ec)									# take a look
bm6 <- blockmodel(valente_n, ec, k=6)			# specifying 6 clusters (worth checking others)
#bm6										# there are a few elements here
plot.blockmodel(bm6)						# take a look
bimage <- bm6$block.model					# pulling off just the block image
gplot(bimage, diag=TRUE, 								# let's plot the image network
      edge.lwd=bimage*5,									# edge-thickness as image-matrix density
      vertex.cex=sqrt(table(bm6$block.membership))/2,		# vertex-size as block size
      vertex.sides=50, label.pos=5, vertex.col="gray",
      label=names(table(bm6$block.membership)))	

plot.sociomatrix(bimage)	# the image matrix
valente_n %v% "blocks" <- cbind(bm6$plabels, bm6$block.membership)[order(cbind(bm6$plabels, bm6$block.membership)[,1]),][,2]	
plot(valente_n, vertex.col="blocks", displaylabels=T)	