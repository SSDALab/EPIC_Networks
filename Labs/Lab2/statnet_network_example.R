
#####################################
### Zack W. Almquist, University of Minnesota
### EPIC - SNA
###
### Networks
###
#####################################



#####################################
###
### Load sna and network packages
###
#####################################

library(sna)      ### A package with classic network metrics and models (a few new ones too!)
library(network)  ### An important class for network data
library(ergm)     ### A package for statistical network models
library(networkMethods)
#####################################
###
### Data types, A reminder!
###
#####################################

## Adjacency Matrix


################################################
#### Example from
#### http://faculty.ucr.edu/~hanneman/nettext/
#### Robert A. Hanneman and Mark Riddle
### Introduction to social network methods
################################################

###########################
## Read in examples from Chapter 5: Using matrices to represent social relations
###########################
require(XML)
theurl <- "http://faculty.ucr.edu/~hanneman/nettext/C5_%20Matrices.html"
tables <- readHTMLTable(theurl,stringsAsFactors = FALSE)
tables


#####################################
###
### Let's look at Figure 5.5
###
#####################################

##### First we need to do some data cleaning
aae<- tables[[4]]      ##asymetric adjacency example
rownames(aae)<-aae[,1] ## Fix rowname issue
aae<-aae[,2:NCOL(aae)] ## Drop first row
diag(aae)<-NA          ## Make diagonal NA
## Change class from character to numeric
for(i in 1:NCOL(aae)){aae[[i]]<-as.numeric(aae[[i]])} 
## Print to see how the matrix looks
aae

## Let's use gplot to visualize the network!
gplot(aae,label=rownames(aae),vertex.cex=.5,edge.col=rgb(0,0,0,.5),edge.lwd=.7, mode="circle")


###########################
## Edgelist
###########################

## Let's view this matrix as an edge list now!
as.edgelist.sna(aae)
el<-as.edgelist.sna(aae)

###########################
## Network Object
###########################

## As a network object
network(aae)
net<-network(aae)

## Let's do a plot using plot.network (notice we don't need to specify .network, why is that?)
plot(net,label="vertex.names",vertex.cex=.5,edge.col=rgb(0,0,0,.5),edge.lwd=.7, mode="circle")


###########################
## Converting between all 3!
###########################

## Converting
aae                  ## Original data is a data.frame object
as.matrix(aae)       ## We can convert it into a matrix
net<-network(aae)    ## As a network object
net
net[,]               ## As an adjacency matrix from a network object
as.sociomatrix(net)  ## As an adjacency matrix from a network object
network(el)          ## Converting Edgelist to network object

#######################################
######
## Notes about Matrix Algebra
######
#######################################

### Convert into matrix
mat<-as.matrix(aae)
diag(mat)<-0
mat

### Elementwise multiplication
mat*mat

### Matrix multiplication
mat%*%mat

### Not the same thing!
mat*mat == mat%*%mat



###########################
## Reading in files
###########################

### read.csv
### read.lines
### read.table
### Discussion!



#####################################
###
### Network Data
###
#####################################

#library(devtools)
#install_github("zalmquist/networkdata")

library(network)
help(package=networkdata)

################
## Class exercise, everybody plot!
################


#####################################
###
### Plotting: Try Some Examples
###
#####################################




#####################################
###
### Simulation: Random Graphs (first introduction to baseline models)
###
#####################################

args(rgraph)

rgraph(5)
rgraph(5,mode="graph")


#######################################
######
## Introduction to mixing matrices and attributes (we will talk about this on wed)
######
#######################################
net%v%"gender"<-c("Male","Female","Male","Female")

mixingmatrix(net, "gender")


#########################################
# More on networkdata
#########################################

# sna can handle network data in many forms.  For instance, the function gden calculates
# network density; we can use it on a network object, an adjacency matrix, a list of
# such matrices, etc.
data(flo)
flo                                             # Adjacency form
gden(flo)
nflo<-network(flo,directed=FALSE)               # Network form
gden(nflo)
gden(list(flo,nflo))                            # Lists of matrices/networks
aflo<-array(dim=c(2,NROW(flo),NROW(flo)))       # Array form
aflo[1,,]<-flo
aflo[2,,]<-flo
gden(aflo)
gden(list(flo,aflo,nflo))                       # Yet more lists

# sna also supports a special kind of matrix called an "sna edgelist."  These are three-
# column matrices, each row of which represents an edge (via its sender, recipient, and
# value, respectively).  sna edgelists have special attributes that indicate their
# size, vertex names (if any), and bipartite status (if applicable).
eflo<-as.edgelist.sna(flo)                      # Coerce flo to an sna edgelist
eflo
attr(eflo,"n")                                  # How many vertices are there?
attr(eflo,"vnames")                             # Are there vertex names?
attr(eflo,"n")<-30                              # Could add isolates....
as.network(eflo)                                # As a network object
as.sociomatrix(as.network(eflo))                # Can transform back w/as.sociomatrix.sna
                     

# sna edgelists can be handy with large data sets (as a simple alternative to network
# objects).  To make one, just add an "n" attribute to a valid three-column matrix!
mat<-cbind(rep(2,4),3:6,rep(1,4))               # Create edges from 2 to 3:6
attr(mat,"n")<-6                                # Set total number of vertices to 6
mat
gden(mat)                                       # Can now pass to sna routines
as.sociomatrix.sna(mat)                         # Can see in adjacency form

# For more information....
?as.edgelist.sna
?as.sociomatrix.sna
?attr
?sna


# 3.2 Network visualization with gplot
data(contig_1993)
data(mids_1993)
# Begin by plotting contiguity among nations in 1993 (from the Correlates of War project)
gplot(contig_1993)                              # The default visualization
gplot(contig_1993, usearrows=FALSE)             # Turn off arrows manually
gplot(contig_1993, gmode="graph")               # Can also tell gplot the data is undirected

# We can add labels to the vertices - network.vertex.names reports them
gplot(contig_1993, gmode="graph",
      label=network.vertex.names(contig_1993))

# This plot is too large/dense for the default settings to work.  Let's refine them.
gplot(contig_1993, gmode="graph", label.cex=0.5, label.col=4,
      label=network.vertex.names(contig_1993))     # Shrink labels and recolor

# Here's an example of directed data - militarized interstate disputes (MIDs) for 1993
gplot(mids_1993, label.cex=0.5, label.col=4,
      label=network.vertex.names(mids_1993))       # Basic display, with labels

# All those isolates can get in the way - we can suppress them using displayisolates
gplot(mids_1993, displayisolates=FALSE, label.cex=0.5, label.col=4,
      label=network.vertex.names(mids_1993))

# The default layout algorithm is that of Frutchterman-Reingold (1991), can use others
gplot(mids_1993, displayisolates=FALSE, label.cex=0.5, label.col=4,
      label=network.vertex.names(mids_1993), mode="circle")   # The infamous circle
gplot(mids_1993, displayisolates=FALSE, label.cex=0.5, label.col=4,
      label=network.vertex.names(mids_1993), mode="mds")      # MDS of position similarity

# When a layout is generated, the results can be saved for later reuse:
coords <- gplot(contig_1993)                     # Capture the magic of the moment
coords                                           # Show the vertex coordinates

#Saved (or a priori) layouts can be used via the coord argument:
gplot(mids_1993, label.cex=0.5, label.col=4, coord=coords,
      label=network.vertex.names(mids_1993))        # Relive the magic

# When the default settings are insufficient, interactive mode allows for tweaking
coords <- gplot(contig_1993, interactive=TRUE)   # Modify and save
gplot(contig_1993, coord=coords, gmode="graph")  # Should reproduce the modified layout

#For more information....
?gplot
?gplot.layout







