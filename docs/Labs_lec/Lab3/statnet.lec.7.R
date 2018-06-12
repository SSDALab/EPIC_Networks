################################################################################
#
#  Network Methods Lecture 7 - R/Statnet Component
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
#
#-Visualization with Categorical Covariates-------------------------------------
#
# Load the Faux Mesa High data set (aka "fauxhigh")
data("fauxhigh")
data("faux.mesa.high")
fauxhigh<-faux.mesa.high

# Display the data, along with covariates
plot(fauxhigh,vertex.col="Sex")        # Plot by gender (covariate is misnamed!)
vals<-sort(unique(fauxhigh%v%"Sex"))   # Let's add a legend....
legend("topleft",fill=1:length(vals),legend=vals,bty="n")

plot(fauxhigh,vertex.col="Grade")      # Now, plot by grade
legend("topleft",fill=7:12,legend=7:12,bty="n")

plot(fauxhigh,vertex.col="Race")       # Finally, plot by race
vals<-sort(unique(fauxhigh%v%"Race"))
legend("topleft",fill=1:length(vals),legend=vals,bty="n")

# For more information....
?fauxhigh
?legend
#
#-Obtaining mixing matrices-----------------------------------------------------
#
# The mixingmatrix command is poorly documented, but very useful.  Let's
# obtain mixing matrices for each of the above attributes: 
mms<-mixingmatrix(fauxhigh,"Sex")               # Compute w/a network object and
mmg<-mixingmatrix(fauxhigh,"Grade")             # attribute name  
mmr<-mixingmatrix(fauxhigh,"Race")
mms                                             # See what we have...
mmg
mmr
names(mms)                                      # This is a complex object
class(mms)
mms$matrix                                      # Extract the mixing matrix

# Can crudely visualize using plot.sociomatrix:
plot.sociomatrix(mms$matrix)                    # Must use matrix, not object
plot.sociomatrix(mmg$matrix)
plot.sociomatrix(mmr$matrix)
#
#-Obtaining expectations and z-scores-------------------------------------------
#
# Let's examine mixing by grade....
mmg
gmarg<-rowSums(mmg$matrix)                       # Marginals for grade
gmarg
emmg<-(gmarg%o%gmarg)/sum(gmarg)                 # Expected mixing matrix
emmg
sum(emmg)==sum(gmarg)==sum(mmg$matrix)           # Confirm: edges preserved
dmmg<-mmg$matrix-emmg                            # Divergence from expectation
dmmg
zmmg<-dmmg/sqrt(emmg)                            # Approximate z-scores
zmmg                                             # Note the strong homophily!

#Plot the z-scores as a reduced form signed blockmodel
gplot(abs(zmmg)>2,edge.col=sign(zmmg)+3,label=7:12,boxed.lab=FALSE,
    usearrows=FALSE,diag=TRUE)                   # All avoid grade 7?

#For more information....
?%o%
#
#-Using the blockmodel function with fixed membership---------------------------
#
# The "blockmodel" function in sna can be used for various sorts of
# blockmodeling; here, we will pass it a membership vector, and use it to
# extract some basic information.
memb<-match(fauxhigh%v%"Race",sort(unique(fauxhigh%v%"Race"))) # Race vector
bm<-blockmodel(fauxhigh,memb)                                  # Block by race
bm
names(bm)                                              # Examine components
bmr<-bm$block.model                                    # Get the block densities

#Plot the reduced form blockmodel
gplot(bmr,edge.lwd=bmr*200,diag=TRUE,usearrows=FALSE)

