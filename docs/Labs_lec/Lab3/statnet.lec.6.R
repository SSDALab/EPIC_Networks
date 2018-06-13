################################################################################
#
#  Network Methods Lecture 6 - R/Statnet Component
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
load("nmlec6.Rdata")

# Load the cug.test helper function (need to get file from class web site)
load("cug.test.Rdata")
#
#-Transitivity and the triad census---------------------------------------------
#
# Get transitivity for the Sampson monastery data
gtrans(sampson)

# How does transitivity compare to density?  (Log-odds method)
log(gtrans(sampson)/gden(sampson))

# Are positive relations more transitive (relative to density) than negative
# ones?  (Compare to last lecture's reciprocity test.)  Let's try a vector
# permutation test:
ltr<-log(gtrans(sampson)/gden(sampson))
ispos<-c(TRUE,FALSE,TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)
obs<-sum(ltr[ispos])-sum(ltr[!ispos])
reps<-vector()
for(i in 1:1e4){
  temp<-sample(ispos)
  reps[i]<-sum(ltr[temp])-sum(ltr[!temp])
}
mean(reps>=obs)                                             # Upper tail p-value
mean(abs(reps)>=abs(obs))                                   # Two-sided version
hist(reps)
abline(v=obs,col=2,lwd=3)                                   # Visualize it

# Now, let's get the triad census for each network
triad.census(sampson)

# Cool trick: two-way correspondence analysis of graphs and their triad census
# scores (aka a "Faust diagram").  Networks here appear close to the triad
# types they contain at excess frequency (distances are chi-squared based; 
# see references in ?corresp for more detail).
library(MASS)                                        # Requires the MASS package
plot(corresp(triad.census(sampson),nf=2))       # Plot network/triad association

# What if this data were symmetric?  We can symmetrize to illustrate.
triad.census(symmetrize(sampson),mode="graph")    #Need to use mode="graph" here

# For more information....
?gtrans
?triad.census
?corresp
?symmetrize
#
#-Simple Univariate CUG Tests---------------------------------------------------
#
# We can use the helper function "cug.test" (not yet in statnet) to make life
# easier when performing CUG tests.  Let's try testing some data on trade in
# complex manufactured goods to see if overall activity (density) is greater
# then would be expected from size alone.

# Anderson, B.S.; Butts, C.T.; and Carley, K.M. (1999). “The Interaction of Size and Density with Graph-Level Indices.” Social Networks, 21(3), 239-267.

## cug.test
#Draw a highly reciprocal network
g<-rguman(1,15,mut=0.25,asym=0.05,null=0.7)

#Test transitivity against size, density, and the dyad census
cug.test(g,gtrans,cmode="size")
cug.test(g,gtrans,cmode="edges")
cug.test(g,gtrans,cmode="dyad.census")


## EMperical Case
data(trade)
ctrade<-trade$CRUDE_MATERIALS
cug.test(ctrade,gden)                 # Call cug.test with the gden (density) function

# Is there more reciprocity than density would suggest?  Let's see.
cug.test(ctrade,grecip,cmode="edges")       # Conditioning on edges, calling grecip

# Given biases in density and reciprocity, we might look to see if there is a
# bias in transitivity, too.  This time, let's condition on all of the above.
cug.test(ctrade,gtrans,cmode="dyad")   #                Conditioning on dyad census

# We are not limited to simple commands.  Let's try indegree centralization:
ct<-cug.test(ctrade,centralization,cmode="dyad",FUN.arg=list(FUN=degree,
    cmode="indegree"))  # Note that we here must pass not only arguments to 
                        # cug.test, but also to centralization and degree!
ct                                                           # Print the result
plot(ct)                                                     # Can also plot it!
                     
