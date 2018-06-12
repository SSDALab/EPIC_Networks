##########################################################################
#	R & igraph Tutorial for EPIC-SNA 2018
#	author: jimi adams
# 	Modified: Zack Almquist
#	last updated: 2018-06-11
#
##########################################################################
library(igraph)

# setwd("Your location here")

###	This block reads in 2 pajek (.net) files & their
###	corresponding attributes file that have been combined 
###	into a single text file.
sch4 <- read.graph("data/sch4.net", format="pajek")	# getting the adjacency matrix
att <- read.table("data/sch4_attr.txt", header=T, sep=" ")
V(sch4)$sex <- att$sex
V(sch4)$race <- att$race
V(sch4)$grade <- att$grade
V(sch4)$school <- att$school
rm(att)

sch23 <- read.graph("data/sch23.net", format="pajek")	# getting the adjacency matrix
att <- read.table("data/sch23_attr.txt", header=T, sep=" ")
V(sch23)$sex <- att$sex
V(sch23)$race <- att$race
V(sch23)$grade <- att$grade
V(sch23)$school <- att$school
rm(att)

###	This block reads in hand-coded adjacency lists that are 
###	in .csv format & include a node attribute.
ex1a <- read.csv("data/simple_undirected.csv", header=T)		# reading in the undirected mixing example
ties<- subset(ex1a,select=c(alt1,alt2,alt3))			# stripping off just the adjacency list
nodes<- subset(ex1a,select=c(id,attr1))					# stripping off just the attributes
df <- data.frame(snd = row(ties)[!is.na(ties)], rcv = ties[!is.na(ties)])	# converting to an edgelist; igraph can natively handle edgelists and adjacency matrices
ex1a_g <- graph.data.frame(df, directed=F, vertices=nodes)	# converting to an igraph “graph” object
ex1a_g <- simplify(ex1a_g)

ex1b <- read.csv("data/simple_directed.csv", header=T)	
nodes <- subset(ex1b,select=c(id,attr1))		
ties <- subset(ex1b,select=c(alt1,alt2,alt3))	
df <- data.frame(snd = row(ties)[!is.na(ties)], rcv = ties[!is.na(ties)])	
ex1b_g <- graph.data.frame(df, directed=T, vertices=nodes)	
rm(ties, ex1a, nodes, ex1b, df)

###	This block is an exmple of a hand coded igraph object.
t_ex <- graph(c(1,2, 2,1, 1,3, 3,1, 2,3, 3,2, 1,4, 1,8, 1,10, 2,10, 2,9, 9,3, 4,6, 7,6, 4,5, 5,4, 9,10, 10,9, 3,4, 4,3, 4,10, 6,11, 11,7))

###	This block reads in a pajek (.net) file & a
###	corresponding attribute file in what is a standard
###	file format for pajek (.clu) files per attribute
valente <- read.graph("data/valente.net", format="pajek")	# getting the adjacency matrix
gender <- as.matrix(read.table("data/valente.clu", skip=1))	# Pajek attributes frequently are in a separate file
V(valente)$gender <- as.vector(gender)					# attaching the attribute, make numeric
rm(gender)

###	This is a function for transforming data formats
require(intergraph)
require(statnet)
v1 <- simplify(valente) 	#removes loops, duplicate ties, etc.
asNetwork(v1) -> valente_n
rm(v1)

###	This is another way to handcode igraph objects
###	it allows you to send/rcv multiple ties from/to the same
### node at the same time (everything on the left side of a
###	dash here will be connected to everything on the right)
cent_ex1a <- graph.formula(x1--x2:x3:x4:a1,a1--c:a2:x2:x3:x4,x2--x3:x4, x3--x4,a2--x5:x6:x7:x8, x5--x6:x7:x8, a2--c:x6:x7:x8, x6--x7:x8, x7--x8)
cent_ex1b <- graph.formula(x1--x2:x3:x4:x5:b1, x2--b2:x3:x4:x5, x3--x4:x5, x4--x5,
							b1--c:a, b2--b3, b3--b5, a--b4, c--b5, b5--x6, b4-x7, 
							x6--x7:x8:x9:x0, x7--x8:x9:x0, x8--x9:x0, x9--x0)
cent_ex1a <- simplify(cent_ex1a)
cent_ex1b <- simplify(cent_ex1b)
cent_ex2 <- graph(c(1,7, 2,7, 3,7, 4,8, 5,8, 6,8, 9,12, 10,12, 11,12, 7,8, 7,12, 7,17, 8,13, 8,18, 13,18, 12,17, 13,14, 13,15, 13,16, 17,18, 17,19, 18,19, 17,20, 18,21, 20,21, 20,19, 21,19, 20,25, 21,26, 22,25, 23,25, 24,25, 26,27, 26,29, 26,28, 26,31, 25,30, 30,31, 30,32, 30,33, 30,34, 31,35, 31,36, 31,37, 20,30, 21,31, 12,25, 13,26), directed=F)

###	This is how you use the intergraph package
### to convert from igraph to network objects.
asNetwork(cent_ex1a) -> c_ex1a
asNetwork(cent_ex1b) -> c_ex1b
asNetwork(cent_ex2) -> c_ex2

###	More examples of reading in pajek (.net) files and their
### corresponding attribute (.clu) files.
korea1 <- read.graph("data/korea1.net", format="pajek")
V(korea1)$adopt <- as.vector(as.matrix(read.table("data/Korea1_adopters.clu", skip=1)))
V(korea1)$club <- as.vector(as.matrix(read.table("data/Korea1_members.clu", skip=1)))

korea2 <- read.graph("data/korea2.net", format="pajek")
V(korea2)$adopt <- as.vector(as.matrix(read.table("data/Korea2_adopters.clu", skip=1)))
V(korea2)$club <- as.vector(as.matrix(read.table("data/Korea2_members.clu", skip=1)))

###	More examples of reading in adjacency list (.csv) files 
### with attributes
ex1a <- read.csv("data/simple_undirected.csv", header=T)		# reading in the undirected mixing example
ties<- subset(ex1a,select=c(alt1,alt2,alt3))			# stripping off just the adjacency list
nodes<- subset(ex1a,select=c(id,attr1))					# stripping off just the attributes
df <- data.frame(snd = row(ties)[!is.na(ties)], rcv = ties[!is.na(ties)])	# converting to an edgelist; igraph can natively handle edgelists and adjacency matrices
ex1a_g <- graph.data.frame(df, directed=F, vertices=nodes)	# converting to an igraph “graph” object
ex1a_g <- simplify(ex1a_g)
rm(df, ex1a, nodes, ties)

###	.gml is another relatively common edgelist format.
###	this partiular example didn't have the attribute we wanted
###	already attached, so I am manually adding that.
karate <- read.graph("data/karate.gml", format="gml")
V(karate)$split <- c(0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,1,0,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1)

###	Here is an example of manually coded network objects
f <- network.initialize(5)
network::add.edges(f, c(1,1,1,1,2,2,2,2,3,3,4,4,5,5), c(2,3,4,5,1,3,4,5,4,5,3,5,3,4))
network::set.vertex.attribute(f, "id", c("P1", "P2", "C1", "C2", "C3"))

ex <- network.initialize(8, directed=F)
network::add.edges(ex, c(1,1,1,2,2,2,3,3,4,4,5,5,6,7), c(3,4,5,3,4,5,6,7,6,7,6,7,8,8))

###	Simply reading in network objects that already exist
###	as an R environment.
load("data/greys.RData")
### In that environment, the attributes are text, some analyses
###	prefer only numeric, so this section converts that.
network::set.vertex.attribute(greys, "nrace", 1, which(network::get.vertex.attribute(greys, "race")=="White"))
network::set.vertex.attribute(greys, "nrace", 2, which(network::get.vertex.attribute(greys, "race")=="Black"))
network::set.vertex.attribute(greys, "nrace", 3, which(network::get.vertex.attribute(greys, "race")=="Other"))

###	These are objects of the format for Siena models.
###	See Tom Snijders's web page for more details.
friend.data.w1 <- as.matrix(read.table("data/s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("data/s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("data/s50-network3.dat"))
drink <- as.matrix(read.table("data/s50-alcohol.dat"))
smoke <- as.matrix(read.table("data/s50-smoke.dat"))

save(list=ls(),file="Tutorial.Rdata")


