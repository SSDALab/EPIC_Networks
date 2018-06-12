##########################################################################
# author: 		jimi adams
#	last updated:	2014-02-17
#	This script computes demonstrates the computation of Burt's "Constraint" measure.
#	The numbers used correspond to slides used in lecture.
#########################################################################

p<- matrix(c(0, .25, .25, .25, .25, 
			.5, 0, 0, 0, .5, 
			1, 0, 0, 0, 0, 
			.5, 0, 0, 0, .5, 
			.33, .33, 0, .33, 0), 
			nrow=5, byrow=T)
p2 <- p %*% p 			#remember this is how we get matrix multiplication (instead of multiplying the matrices elementwise)
diag(p2) <- 0 			#we typically don't think of one constraining themselves
pp2 <- p + p2
pp2sq <- pp2 * pp2		# this time I DO want them multiplied element wise (we're just squaring the values)
################################################################
# Next is the tricky part - the measure if you look at the formula for c only applies to pairs to which ego is connected.
# So we're going to discount the elements that are estimating constraint from those not connected to the ego in question).
# (See the sum only being for all ij pairs, not for all nodes in the graph.)
# It's worth noting that there is a variant of this measure that woudl keep these details around, but not the one I
# presented / your reading described.
################################################################
pp2sq[2,3] <- 0
pp2sq[2,4] <- 0
pp2sq[3,2] <- 0
pp2sq[3,4] <- 0
pp2sq[3,5] <- 0
pp2sq[4,2] <- 0
pp2sq[4,3] <- 0
pp2sq[5,3] <- 0

constr <- apply(pp2sq, 1, sum)		# This applies the "sum" function across the rows, 2 in the second input would sum by columns

################################################################
# The igraph version is a LOT easier, but it's worthwhile to demonstrate what the computation is doing.
################################################################
g <- graph(c(1,2, 1,3, 1,4, 1,5, 2,5, 4,5), directed=F)
constraint(g) 						# The only differences from our result above stem from some rounding errors.
