################################################################################
#
#  Network Methods Lecture 5 - R/Statnet Component
#
#  SOC 280 - Analysis of Social Network Data
#  Carter T. Butts, University of Califorina, Irvine
#  Spring Quarter, 2009
#
################################################################################
#
#-A reminder on getting started....---------------------------------------------
#
library(statnet)                      # Load the statnet library

# Load today's data set
load("nmlec5.Rdata")
#
#-Reciprocity and the dyad census-----------------------------------------------
#
# Get dyadic recprocity for the Sampson monastery data
grecip(sampson)

# Several relations have identical indices.  The dyad census shows why:
dc<-dyad.census(sampson)                       # Get dyad census
dc                                             # Several have same # Asyms
(dc[,1]+dc[,3])/rowSums(dc) == grecip(sampson) # Confirm the definition

# Try edgewise reciprocity (more useful, in general)
grecip(sampson,measure="edgewise")             # Calculate the measure
2*dc[,1]/(2*dc[,1]+dc[,2])                     # Show it the "hard way"

# Compare edgewise reciprocity to density ("r4" in the Butts article)
log(grecip(sampson,measure="edgewise")/gden(sampson))

# Are positive relations more reciprocal (relative to density) than negative
# ones?  Let's try a simple permutation test:
r4<-log(grecip(sampson,measure="edgewise")/gden(sampson))
ispos<-c(TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
obs<-sum(r4[ispos])-sum(r4[!ispos])
reps<-vector()
for(i in 1:1e4){
  temp<-sample(ispos)
  reps[i]<-sum(r4[temp])-sum(r4[!temp])
}
mean(reps>=obs)                                             # Upper tail p-value
mean(abs(reps)>=abs(obs))                                   # Two-sided version
hist(reps)
abline(v=obs,col=2,lwd=3)                                   # Visualize it
#
#-Hierarchy and centralization--------------------------------------------------
#
# Hierarchy is the flip side of reciprocity.  Let's try it here:
hierarchy(sampson)                       # Default measure: 1-dyadic reciprocity
1-grecip(sampson)                        # Manually confirm
hierarchy(sampson,measure="krackhardt")  # Krackhardt's non-local measure
# Visualize the difference:
plot(hierarchy(sampson),hierarchy(sampson,measure="krackhardt"))

# What about centralization?
centralization(sampson,degree,cmode="indegree")    #Indegree centralization
centralization(sampson,degree,cmode="outdegree")   #Outdegree centralization
centralization(sampson,betweenness)                #Betweenness centralization
#
#-Elementary random graph generation--------------------------------------------
#
rgraph(10)                               # A uniform random digraph of order 10
rgraph(10, tp=3/9)                       # Homogeneous Bernoulli w/mean degree 3
rgraph(10, tp=3/9, mode="graph")         # As above, but undirected
rgnm(1, 10, 20)                          # Uniform conditional on order, edges
rguman(1, 10, mut=0.5, asym=0.25, null=0.25)  # Homogeneous multinomial on 
                                              # dyad census
rguman(1, 10, mut=0, asym=1, null=0)     # An extreme case: random tournament
grecip(rguman(1,10,mut=0,asym=1,null=0)) # Note the asymmetry....

gplot3d(rgws(1,50,1,2,0))                # A Watts-Strogatz process - baseline
gplot3d(rgws(1,50,1,2,0.05))             # ...with rewiring probability 0.05
gplot3d(rgws(1,50,1,2,0.2))              # ...with rewiring probability 0.2
#
#-Random graphs conditional on observed statistics------------------------------
#
# Begin by drawing a uniform digraph with the same order as the "praise" network
g.un<-rgraph(network.size(sampson[[9]]))

# Now, try a uniform digraph with the same expected number of edges
g.unem<-rgraph(network.size(sampson[[9]]),tp=gden(sampson[[9]]))

# How about a uniform digraph with the same exact number of edges?
g.unm<-rgnm(1,network.size(sampson[[9]]),network.edgecount(sampson[[9]]))

# We can also try the same expected dyad census...
g.uneman<-rguman(1,network.size(sampson[[9]]),mut=dc[9,1],asym=dc[9,2],
    null=dc[9,3])

# ...and, finally, the same exact dyad census
g.unman<-rguman(1,network.size(sampson[[9]]),mut=dc[9,1],asym=dc[9,2],
    null=dc[9,3],method="exact")

# Plot all of the above, along with the original graph
par(mfrow=c(2,3))                                   # Create a 2x3 panel display
plot(sampson[[9]],main="Praise")
gplot(g.un,main="U|N")
gplot(g.unem,main="U|N,E(M)")
gplot(g.unm,main="U|N,M")
gplot(g.uneman,main="U|N,E(MAN)")
gplot(g.unman,main="U|N,MAN")
par(mfrow=c(1,1))                                   # Reset the display

#For more information....
?rgbn
?rgmn
?rgraph
?rguman
?rgws
?par
