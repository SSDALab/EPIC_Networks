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
#	1.1. 	Node Degree 
##########################################################################
is.directed(valente)
plot.igraph(valente, vertex.color=V(valente)$gender*2, edge.arrow.size= .5, vertex.size=5, vertex.label=NA, layout=layout.fruchterman.reingold)
v_deg <- degree(valente)								# vector of degrees, ignoring direction
v_indeg <- degree(valente, mode="in")					# in-degree vector
v_outdeg <- degree(valente, mode="out")					# out-degree vector
v_deg==v_indeg + v_outdeg								# checking to be sure they're right

adjmat <- get.adjacency(valente)						# pulling off the adjacency matrix
adjmat													# not a standard format
adjmat <- as.matrix(get.adjacency(valente))				# pulling off the adjacency matrix
indeg <- colSums(adjmat)								# getting in-degree
outdeg <- rowSums(adjmat)								# getting out-degree
indeg==v_indeg											# just checking
outdeg==v_outdeg										# just checking

##########################################################################
#	1.2. 	Degree Distribution 
##########################################################################
par(mfrow=c(2,2))                                       # Set up a 2x2 display
hist(v_indeg, xlab="Indegree", main="Indegree Distribution", prob=T)
hist(v_outdeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
hist(v_deg, xlab="Total Degree", main="Total Degree Distribution", prob=TRUE)
plot(v_indeg~v_outdeg)									# Scatter plot
abline(lm(v_indeg~v_outdeg), col="red")					# Reg line
par(mfrow=c(1,1))  										# Restoring the plot

degtab <- table(v_deg)
plot(names(degtab), degtab, log="xy", yaxt="n")
axis(2, at=(seq(1,max(degtab+1),1)))					# Log plot axes labels need some help

plot(v_indeg~v_outdeg, col=c("red", "black")[V(valente)$gender+1])
# They're stacked, so lets add a little noise just to be sure
plot(jitter(v_indeg, factor=.5)~jitter(v_outdeg, factor=.5), col=c("red", "black")[V(valente)$gender+1])
t.test(v_deg~V(valente)$gender)
t.test(v_indeg~V(valente)$gender)
t.test(v_outdeg~V(valente)$gender)

##########################################################################
#	2.1. 	Network Composition 
##########################################################################

# check might need data/data if windows
source("data/scripts/IQV.R")	# don’t forget the quotes! With no directory specified, it simply looks in your wd
v_iqv_gender <- iqv(valente, "gender")
V(valente)$gend_var <- v_iqv_gender$egonet
plot.igraph(valente, vertex.color=V(valente)$gender*2, edge.arrow.size= .5, vertex.size=2+5*V(valente)$gend_var, vertex.label=NA, layout=layout.fruchterman.reingold)

alter_mean <- function(graph, attribute, mode="all"){				# mode can also be "in" or "out"
	mean_attr <- list(0)											# initializing a list
	for (i in 1:vcount(graph)){										# for all nodes in school
		ineighbors <- neighbors(graph, i, mode)						# find the neighbors
		attr <- get.vertex.attribute(graph, attribute, ineighbors)	# finding the neighbors' attribute
		mean_attr[[i]] <- mean(attr, na.rm=T)						# averaging over the list
	}
	return(unlist(mean_attr))										# returning it as a vector
}
s4_grademean_rcv <- alter_mean(sch4, "grade", "in")
mean(s4_grademean_rcv)
s4_grademean_snd <- alter_mean(sch4, "grade", "out")
mean(s4_grademean_snd, na.rm=T)

##########################################################################
#	2.2.1. 	Density 
##########################################################################
gu <- graph(c(1,2, 2,3, 3,4, 3,5, 4,5), directed=F)		# toy undirected example from slides
adjmat <- get.adjacency(gu)					# get adjacency matrix
T <- sum(adjmat)							# ties observed
P <- vcount(gu)*(vcount(gu)-1)/2			# ties possible
T/P/2										# density
adjmat <- get.adjacency(gu, "upper")		# grabbing only the top half
T <- sum (adjmat)							# ties observed (possible doesn't change from above)
T/P											# density
graph.density(gu)							# same, via igraph's function

gd <- graph(c(1,2, 2,1, 3,2, 3,4, 4,3, 3,5, 4,5))		# toy directed example from slides
adjmat <- get.adjacency(gd)					# get adjacency matrix
T <- sum(adjmat)							# ties observed
P <- vcount(gd)*(vcount(gd)-1)				# ties possible (note the difference)
T/P											# density
graph.density(gd)							# same, via igraph's function

is.directed(valente)						# just to remind ourselves
adjmat <- get.adjacency(valente)			# grabbing the adjacency matrix
T <- sum(adjmat)							# ties observed
P <- vcount(valente)*(vcount(valente)-1)	# ties possible
T/P											# density
graph.density(valente)						# with igraph's function

##########################################################################
#	3.1 	Distance (from matrices - in-class examples)
##########################################################################
adjmat <- get.adjacency(ex1a_g)							# get adjacency matrix
twopaths <- adjmat %*% adjmat							# how many 2-paths
anytwo <-ifelse(twopaths>0, 2, 0)						# are there any?
threepaths <- twopaths %*% adjmat 						# how many 3-paths
anythree<-ifelse(threepaths>0, 3, 0)					# are there any?
anyone <- as.numeric(adjmat)							# not really needed
paths<- apply(cbind(anyone, anytwo, anythree), 1, function(x) min(x[x>0])) #find non-zero minimum
paths <- matrix(paths, nrow=7, ncol=7)					# return to matrix
diag(paths) <- 0										# remove diagonal
paths

#### Here's where I left off on June 6th ####
ex1b_g <- delete.edges(ex1b_g, 7)
ex1b_g <- add.edges(ex1b_g, c(5,4))
adjmat <- get.adjacency(ex1b_g)							# get adjacency matrix
twopaths <- adjmat %*% adjmat							# how many 2-paths?
anytwo <-ifelse(twopaths>0, 2, 0)						# are there any?
threepaths <- twopaths %*% adjmat						# how many 3-paths?
anythree<-ifelse(threepaths>0, 3, 0)					# are there any?
fourpaths <- threepaths %*% adjmat						# how many 4-paths?
anyfour<-ifelse(fourpaths>0, 4, 0)						# are there any?
anyone <- as.numeric(adjmat)							# mostly unnecessary
paths<- apply(cbind(anyone, anytwo, anythree, anyfour), 1, function(x) min(x[x>0]))	# find non-zero minimum
paths <- matrix(paths, nrow=7, ncol=7)					# return to matrix form
diag(paths) <- 0										# remove the diagonal
paths													# and voila

##########################################################################
#	3.2 	Distance (from igraph functions)
##########################################################################
diameter(ex1a_g)
diameter(ex1b_g)		
shortest.paths(ex1a_g)									# much easier
shortest.paths(ex1b_g, mode="out")						# much easier

v_distance <- shortest.paths(valente, mode="out")		# no output

pairs <- length(v_distance) - length(diag(v_distance))	# number of odered pairs
unreachable <- length(which(v_distance=="Inf"))			# number of unreachable ordered paths
unreachable/pairs										# proportion of all pairs unreachable

diameter(valente)										# the longest shortest path
max(v_distance[which(is.finite(v_distance))])			# should equal the diameter
v_distance[which(v_distance=="Inf")] <- vcount(valente) - 1		# here N-1

group1_distances <- shortest.paths(valente, 
	v=V(valente)[V(valente)$gender==1], 						# Only looking at ties from group 1
	to=V(valente)[V(valente)$gender==1], mode="out") 			# to group 1
rownames(group1_distances) <- V(valente)[V(valente)$gender==1]	# rather than matrix row #s
colnames(group1_distances) <- V(valente)[V(valente)$gender==1]	# rather than matrix col #s
group1_distances


##########################################################################
#	2.2.2. 	Local Graph Connectivity (Omitted from section above)
##########################################################################
g <- graph(c(1,2, 1,3, 1,4, 1,5, 2,5, 5,4), directed=F)
constraint(g)								# this gives Burt's constraint measure

effective.size <- function(g, ego, mode="all") {		# igraph doesn't have an "effective size" command
	n <- degree(g, mode=mode)[ego]						# ego's degree
	es <- n												# initializing effective size
	ns <- neighbors(g,ego, mode=mode)					# identifying ego's neighborhood
	if(n>0){
		for (j in 1:n){									# looping over everyone in ns
			nsns <- neighbors(g,ns[j], mode=mode)		# finding neighbors' neighbors
			r <- length(intersect(ns, nsns))			# only those also in ego's neighborhood
			es <- es - (r/n)							# subtracting redundancies
		}
	}
	return(es)
}
es <- rep(0,vcount(g))									# creating an empty vector for es scores
for(i in 1:5){effective.size(g,i)}->es[i]				# for all nodes in example
es														# did we get the same result?
size <- degree(g)										# Grabbing degree to compute:
efficiency <- es/size									# "Efficiency"
efficiency

v_es <- rep(0,vcount(valente))							# creating an empty vector for es scores
for (i in 1:vcount(valente)){							# for all nodes in valente
	v_es[i] <- effective.size(valente,i, mode="out")	# computing OUT-degree effective size
}	
v_size <- degree(valente)								# nodal degree
v_efficiency <- v_es/v_size								# did we get the same result?
#constraint(valente)									# what do they look like
v_es	
v_efficiency	

##########################################################################
#	4.1 	Social Balance - Dyadic
##########################################################################
g <- graph(c(1,2, 2,1, 2,3, 3,1, 4,1, 4,3, 3,4)) 
dyad.census(g)
reciprocity(g, mode="ratio")					# ratio, because the default includes Nulls
reciprocity(ex1b_g, mode="ratio")
dyad.census(ex1b_g)				

##########################################################################
#	4.2 	Social Balance - Triadic
##########################################################################
triad.census(g)
census <- function(g){
	census_labels = c(	'003',					# no arcs
				'012',							# 1 arcs
				'102','021D','021U','021C',		# 2 arcs
				'111D','111U','030T','030C',	# 3 arcs
				'201','120D','120U','120C',		# 4 arcs
				'210',							# 5 arcs
				'300')							# 6 arcs	
	tcens <- igraph::triad.census(g)
	#tcens[1] <- vcount(g)*(vcount(g)-1)*(vcount(g)-2)/6	- sum(tcens, na.rm=T)	#correcting the NaN result
	tcens <- data.frame(id=census_labels, triads=tcens)
	return(tcens)
}
census(g)
census(ex1b_g)
plot(t_ex)
tcens <- census(t_ex)
tcens
dyad.census(t_ex)

##########################################################################
#	4.3 	Social Balance - Computing Baseline
##########################################################################
#install.packages("sna")
library(sna)
sim.nets <- rguman(100, igraph::vcount(t_ex), mut=.11, asym=.2, null=.69)
exp_t_ex <- sna::triad.census(sim.nets)				# taking the triad census (this is the sna version, but it’s results are the same)
tcens$mean_exp <- apply(exp_t_ex, 2, mean)		# mean of simulated values
tcens$sd_exp <- apply(exp_t_ex, 2, sd)			# sd of simulated values
tcens$tau <- (tcens$triads-tcens$mean_exp)/tcens$sd_exp
tcens	

##########################################################################
#	4.4 	Social Balance - Real Data
##########################################################################
vcens <- census(valente)
igraph::dyad.census(valente)
sim.nets <- rguman(100, igraph::vcount(valente), mut=34/666, asym=77/666, null=555/666)
exp_v <- sna::triad.census(sim.nets)	
vcens$mean_exp <- apply(exp_v, 2, mean)	# mean of simulated values
vcens$sd_exp <- apply(exp_v, 2, sd)	# sd of simulated values
vcens$tau <- (vcens$triads-vcens$mean_exp)/vcens$sd_exp
vcens

