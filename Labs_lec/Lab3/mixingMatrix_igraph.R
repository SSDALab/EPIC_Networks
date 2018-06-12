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
#	2.3.1 Mixing Matrix examples (toy data)
##########################################################################
source("scripts/mixmat.R")	# don’t forget the quotes! With no directory specified, it simply looks in your wd.
plot.igraph(ex1a_g, vertex.color=V(ex1a_g)$attr1*2)
mixmat(ex1a_g, 'attr1', use.density=F)	# the density option would provide proportional representation
plot.igraph(ex1b_g, vertex.color=V(ex1b_g)$attr1*2)
mixmat(ex1b_g, 'attr1', use.density=F)	# for the directed graph

adjmata <- as.matrix(get.adjacency(ex1a_g))		# getting the adjacency matrix
attr1 <- V(ex1a_g)$attr1					# grabbing the attribute
attr1[7] <- 1							# my data file didn't have the attribute for node 7
attra <- cbind(attr1, ifelse(attr1==0,1,0))		# we need the second attribute
adjmata		# just to be sure they're what we expect
attra		# just to be sure they're what we expect
alter_attra <- adjmata %*% attra				# matrix multiplication
alter_attra	# did it work?
t(attra) %*% alter_attra					# now getting the mixing matrix
mixmat(ex1a_g, 'attr1', use.density=F)			# comparing to the result from above

adjmatb <- as.matrix(get.adjacency(ex1b_g))		# getting the adjacency matrix
attr1 <- V(ex1b_g)$attr1					# grabbing the attribute
attr1[7] <- 1							# my data file didn't have the attribute for node 7
attrb <- cbind(attr1, ifelse(attr1==0,1,0))		# we need the second attribute
alter_attrb <- adjmatb %*% attrb				# matrix multiplication
t(attrb) %*% alter_attrb					# now getting the mixing matrix
mixmat(ex1b_g, 'attr1', use.density=F)			# comparing to the result from above
##########################################################################
#	2.3.2. Now, with real data
##########################################################################
plot.igraph(valente, vertex.color=V(valente)$gender*2, layout=layout.fruchterman.reingold, vertex.size=3, vertex.label=NA, edge.arrow.size=.5)	# what does it look like?
mixmat(valente, 'gender', use.density=F)				# one more mixing matrix

adjmat <- as.matrix(get.adjacency(valente))			# pulling off the adjacency matrix
valente_un <- simplify(graph.adjacency(adjmat+t(adjmat)))	# symmetrizing it
V(valente)$gender -> V(valente_un)$gender				# reattaching the gender attribute
mixmat(valente_un, 'gender', use.density=F)			# and voila

##########################################################################
#	2.3.3. A Simulated Random Network
##########################################################################
rg <- erdos.renyi.game(length(V(valente)), length(E(valente)), type="gnm", directed=T)	# a random graph with the same number of nodes & edges as the Valente graph above
V(rg)$gender <- sample(c(0,1), length(V(rg)), replace=T, prob=c(22/37, 15/37))	#randomly assigning gender, matching the probabilities from the Valente graph
mixmat(rg, 'gender', use.density=F)	# If that’s repeatedly highly assortative, we’re in trouble (occasionally it will be)!

