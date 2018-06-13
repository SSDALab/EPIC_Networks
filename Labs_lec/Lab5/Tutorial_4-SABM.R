##########################################################################
#	R & RSiena Tutorial for EPIC-SNA 2017
#	author: jimi adams
#	last updated: 2016-05-13
#
#	NOTE: This is just the code from the full tutorial - distributed as a .pdf.
##########################################################################

##########################################################################
#	0. 	Setting up your R environment
##########################################################################
setwd("~/Dropbox/EPIC_2017/tutorial") 	# Path for ALL files w/location unspecified
#install.packages("RSiena")			# The first time you install a package
library(RSiena)
#install.packages("snow")			# The first time you install a package
library(snow) 
#install.packages("rlecuyer")		# The first time you install a package
library(rlecuyer)
library(network)
library(sna)

load("data/Tutorial.Rdata")
ls()
class(friend.data.w1)				# What format are these data in?
class(drink)

##########################################################################
#	1. 	This script runs a pretty basic Siena model.
#			It is only slightly modified from a version available via Tom Snijders's webiste:
#			http://www.stats.ox.ac.uk/~snijders/siena/
#			where you can also find additional helpful scripts and documentation.
##########################################################################

# look at the data
print(drink)
apply(drink,2,table)  # frequencies for each time point "2" applies the table function within columns [use "1" for rows])
apply(smoke,2,table)

net1 <- as.network(friend.data.w1)
net2 <- as.network(friend.data.w2)
net3 <- as.network( friend.data.w3)
par(mfrow=c(1,3))
plot( net1, xlab = 'friendship t1')
plot( net2, xlab = 'friendship t2')
plot( net3, xlab = 'friendship t3')
par(mfrow=c(1,1))

# Let's add the attributes to the network data for those plots
set.vertex.attribute(net1, "drink", drink[,1])
set.vertex.attribute(net2, "drink", drink[,2])
set.vertex.attribute(net3, "drink", drink[,3])
net1 %v% "smoke" <- smoke[,1]	# a different way to do the same
net2 %v% "smoke" <- smoke[,2]	# a different way to do the same
net3 %v% "smoke" <- smoke[,3]	# a different way to do the same
par(mfrow=c(1,3))
plot(net1, vertex.cex=0.5*get.vertex.attribute(net1, "drink"), vertex.col= "smoke", xlab = 'friendship t1')
plot(net2, vertex.cex=0.5*get.vertex.attribute(net2, "drink"), vertex.col= "smoke", xlab = 'friendship t2')
legend("top", "Size = Drinking Frequency, Color = Smoking Frequency")
plot(net3, vertex.cex=0.5*get.vertex.attribute(net3, "drink"), vertex.col= "smoke", xlab = 'friendship t3')
par(mfrow=c(1,1))


# specify data and model (in a format that RSiena will recognize).
friendship <- sienaNet(array(c(friend.data.w1, friend.data.w2, friend.data.w3 ), dim = c(50, 50, 3)), type="oneMode")  
drinkingbeh <- sienaNet(drink, type = "behavior")	# as a dependent variable
smoke1 <- varCovar(smoke)							# as a time-varrying covariate
#gend <- coCovar(gender)							# if instead we wanted a constant covariate

mydat <- sienaDataCreate (friendship, smoke1, drinkingbeh)	# putting them all together
myeff <- getEffects(mydat)									# this contains the model specification, with defaults
print01Report(mydat, myeff, modelname = 's50_3_init')		# find this file (in your wd) & take a look

cbind(myeff$effectName,myeff$shortName) 	# RSiena uses "shortNames" in defining some effects, let's have a look

# add effects to the model
myeff <- includeEffects(myeff, transTrip, cycle3 )
myeff <- includeEffects(myeff, egoX, altX, simX, interaction1 = "smoke1" )  # makes explicit which X's we're after
myeff <- includeEffects(myeff, egoX, altX, simX, interaction1 = "drinkingbeh" )
myeff <- includeEffects(myeff, name = "drinkingbeh", avAlt, indeg, outdeg, interaction1 = "friendship" )
#myeff <- includeEffects(myeff, cycle3, include=F)	# if you wanted to remove an effect from the model
myeff  # what have we done to this thing

# Setting up the model evaluation, and defining a file to write results out to.
myCoEvModel <- sienaModelCreate( useStdInits = TRUE, projname = 'model_1' ) # projname is the output file 

ans <- siena07( myCoEvModel, data = mydat, effects = myeff)
#ans <- siena07( myCoEvModel, data = mydat, effects = myeff, batch=T, verbose=F)	# suppressing the visual console
ans2 <- siena07(myCoEvModel, data=mydat, effects=myeff, prevAns=ans)	#rerun, starting from where we left off

# A few Addenda

# siena will run faster if your computer has multiple processors (more than 2) 
# with K processors use the following specification; replace "#" with K-1
#ansMP <- siena07( myCoEvModel, data = myCoEvolutionData, effects = myCoEvolutionEff, useCluster=TRUE, nbrNodes=3, initC=TRUE)
#print(ansMP)

# another specification - drops indegree and outdegree effects on behavior
#  these results will be appended to the file specified by the myCoEvModel function
#myeff2 <- includeEffects( myeff, name = "drinkingbeh", indeg, outdeg, interaction1 = "friendship", include=F)
#ans3 <- siena07( myCoEvModel, data = mydat, effects = myeff2)
#print(ans3)

