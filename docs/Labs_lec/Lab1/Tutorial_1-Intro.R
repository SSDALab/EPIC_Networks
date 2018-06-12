##########################################################################
#	R & RSiena Tutorial for EPIC SNA - 2018
#	author: jimi adams
#   Modified: Zack Almquist
#	last updated: 2018-06-11
#
##########################################################################
# 2.1 The Basics
# Interactive Usage
3+4						# evaluation
a <- 3 + 4				# assignment 
3+4 -> a				# same as above 
a						# evaluation
b = 3+2					# you can also assign with the equal sign with the right side being assigned to the left, not recommended
b						# evaluation
a==b					# evaluation "is equal to"
a!=b					# evaluation "not equal"

# Functions
d<-sqrt(49)				# square root
d==a					# evaluation
d==b					# evaluation
my.function <- function(x,y) {
	x^2 + y
	}					# A user defined function with 2 inputs
e <- my.function(a,b) 	# assignment (we initialized a & b above)
e						# evaluation

# Object management
ls()					# list all objects in the workspace
rm(e)					# remove a specific object
ls()					# list all objects in the workspace
#rm(list=ls())			# removes all objects in workspace, USE W/CARE!

# Calling Help Files
help.start()				# launch help in web browser
help(sqrt) 					# get help for specified function
?sqrt						# same as above
help.search('square root')	# search help pages for topics
??sqrt						# same as help.search()

#2.2. Object classes
class(a)											# tells you the class of any object
e <- c(a, b, d)										# c() combines elements into a vector
f <- c("This", "is", "a", "character", "vector")	# can also be used on character objects
g <- c(e, f)										# what happens if we combine classes?
e													# evaluate
f													# evaluate
g													# evaluate
class(g)											# tells you the class of any object

seq <- c(1:10)					# a sequence from 1 to 10, by 1
seq2 <- seq(2,10, by=2)			# assigning a sequence with increments !=1
seq2rev <- seq(10,2, by=-1)		# can also be reversed
seq_twice <- rep(1:5,times=2)	# repeat a sequence twice
seq_dbl <- rep(1:5,each=2)		# repeat a sequence twice, elementwise

seq_dbl[4]					# the 4th element in the assigned vector seq_dbl
seq[-c(4:5)]				# the negative operator removes specified element(s) from a vector
seq >5						# is each element in seq greater than 5
any(seq>5)					# are ANY elements of seq greater than 5
all(seq>5)					# are ALL elements of seq greater than 5
which(seq>5)				# returns indices of elements that are TRUE

h<-matrix(data=c(1:36), nrow=6)		# defines a matrix, number of columns inferred, NOTE that the default is to h				
h[4,2]								# returns the element in the 4th row, 2nd column
h[3,]								# returns the 3rd row as a vector
h[,3]								# returns the 3rd column as a vector
#h[,-4]								# removes the 4th column (surpressed)
#h[-2,]								# removes the 2nd row (surpressed)
h[3,6] <- 0							# replace an element
t(h)								# transpose a matrix (swap rows & columns)
#h %*% t(h)							# matrix multiplication (no output)

i <- data.frame(age=31:35, job=c(T,T,T,T,F),name=LETTERS[1:5])							# assign
i[,2]																					# works with standard matrix operators
i$name																					# the $ operator allows calls by labels
#i <- data.frame(age=31:35, job=c(T,T,T,T,F),name=LETTERS[1:5], stringsAsFactors=F)		# getting rid of "factors"

#setup
setwd("YOUR LOCAL PATH HERE") # Path for ALL files w/location unspecified
#install.packages("igraph")	# The first time you install a package
library(igraph)				# Attach the library to your session
#update.packages("igraph")	# Gets the most recent package version

list.files()																# shows everything in the working directory
list.files("data/")

ex1a <- read.csv("data/simple_undirected.csv", header=T)	 						# reading in the undirected mixing example
ex1a																		# what does it look like?
ties<- subset(ex1a,select=c(alt1,alt2,alt3))								# stripping off just the adjacency list
nodes<- subset(ex1a,select=c(id,attr1))										# stripping off just the attributes
df <- data.frame(snd = row(ties)[!is.na(ties)], rcv = ties[!is.na(ties)])	# converting to an edgelist; igraph can natively handle edgelists and adjacency matrices
df

ex1a_g <- graph.data.frame(df, directed=F, vertices=nodes)					# converting to an igraph “graph” object
plot.igraph(ex1a_g, vertex.color=V(ex1a_g)$attr1*2)							# just to be sure it’s the same, *2 is to avoid using the same color for the label/node color

ex1b <- read.csv("data/simple_directed.csv", header=T)	
nodes <- subset(ex1b,select=c(id,attr1))		
ties <- subset(ex1b,select=c(alt1,alt2,alt3))	
df <- data.frame(snd = row(ties)[!is.na(ties)], rcv = ties[!is.na(ties)])	
df
ex1b_g <- graph.data.frame(df, directed=T, vertices=nodes)	
plot.igraph(ex1b_g, vertex.color=V(ex1b_g)$attr1*2)
		
V(ex1b_g)$attr1				# lists the specified vertex attribute
E(ex1a_g)					# gives the edgelist of the specified graph
simplify(ex1a_g) -> ex1a_g	# getting rid of multiple edges
simplify(ex1b_g) -> ex1b_g	# this command also removes any loops

exa_g <- graph(c(1,3, 2,3, 3,4, 4,5, 4,6, 5,6), directed=F)				# each edge is separated by a comma
V(exa_g)$attr1 <- c(1,1,1,0,0,0)										# attach the attribute vector
plot.igraph(exa_g, vertex.color=V(exa_g)$attr1*2)						# how’s it look?
exb_g <- graph(c(1,3, 3,2, 3,4, 4,3, 4,5, 4,6, 5,6, 6,5), directed=T)	# each edge is separated by a comma
V(exb_g)$attr1 <- c(1,1,0,0,1,0)										# attach the attribute vector
plot.igraph(exb_g, vertex.color=V(exb_g)$attr1*2)						# how’s it look?

valente <- read.graph("data/valente.net", format="pajek")		# getting the adjacency matrix
gender <- as.matrix(read.table("data/valente.clu", skip=1))		# Pajek attributes frequently are in a separate file
V(valente)$gender <- as.vector(gender)						# attaching the attribute
plot.igraph(valente, vertex.color=V(valente)$gender*2, layout=layout.fruchterman.reingold)	# what does it look like?
