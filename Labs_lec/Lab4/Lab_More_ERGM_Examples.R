##################################################################
##
## Zack W Almquist, University of Minnesota
## EPIC SNA _ ERGM Supplement
## 
#####
## Adapted from Carter Butts, UCI Social Network Course 
## and Zack Almquist, UMN Social Network Thoery and Methods course
##
##################################################################



## Required packages
library(ergm)
library(sna)
library(coda)
library(networkdata)





###################
### Example: emon
###################
data(emon)

# Try fitting a triangle model to the Cheyenne case:
fit<-ergm(emon[[1]]~edges+triangle)

# Check the simulation using mcmc.diagnostics (requires coda)
mcmc.diagnostics(fit)                                   # Result is degenerate!

# Here's another example: an instar model w/out burn-in or thinning
fit<-ergm(emon[[1]]~edges+istar(2),interval=1,burnin=0)
mcmc.diagnostics(fit)                                   # Poor convergence

# Will restoring thinning/burn-in help?  (Yes, it will.)
fit<-ergm(emon[[1]]~edges+istar(2))
mcmc.diagnostics(fit)                                   # Much better!
mcmc.diagnostics(fit,center=FALSE)                      # Same, w/out centering
summary(emon[[1]]~edges+istar(2))                       # Compare vs. observed
plot(as.matrix(fit$sample))                             # Can plot directly
abline(v=0,h=0,col=2,lty=2)                             # y_obs is at origin

#
#-Goodness-of-Fit/Adequacy Revisited--------------------------------------------
#
# Our edges/instar model mixes OK, so let's see how good it is
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
fit.gof                                                 # Examine p-values
par(mfrow=c(2,2))
plot(fit.gof)                                           # Distribution plots

# Not as bad we might expect.  But let's see if we can do better by
# adding some covariates to the mix...
fit2<-ergm(emon[[1]]~edges+istar(2)+nodemix("Location")+
             nodeicov("Decision.Rank.Score")+nodeicov("Command.Rank.Score")+
             nodematch("Sponsorship"))
fit2.gof<-gof(fit2,GOF=~idegree+odegree+espartners+distance)
fit2.gof                                                # Examine p-values
par(mfrow=c(2,2))
plot(fit2.gof)                                          # Distribution plots

# Even better.  Would converting Sponsorship to an ofactor help with outdegree?
fit3<-ergm(emon[[1]]~edges+istar(2)+
             nodemix("Location")+nodeicov("Decision.Rank.Score")+ 
             nodeicov("Command.Rank.Score")+nodeofactor("Sponsorship"))
fit3.gof<-gof(fit3,GOF=~idegree+odegree+espartners+distance)
fit3.gof                                                # Examine p-values
par(mfrow=c(2,2))
plot(fit3.gof)                                          # Distribution plots

# Here's a more parsimonous model which also works reasonably well:
fit4<-ergm(emon[[1]]~edges+ostar(2)+nodeicov("Command.Rank.Score")+
             nodeofactor("Sponsorship"))
fit4.gof<-gof(fit4,GOF=~idegree+odegree+espartners+distance)
fit4.gof                                                # Examine p-values
par(mfrow=c(2,2))
plot(fit4.gof)                                          # Distribution plots

###############
## More emon
###############

# Try fitting a triangle model to the Cheyenne case:
fit<-ergm(emon[[1]]~edges+triangle)

# Check the simulation using mcmc.diagnostics (requires coda)
mcmc.diagnostics(fit)                                   # Result is degenerate!

# Here's another example: an instar model w/out burn-in or thinning
fit<-ergm(emon[[1]]~edges+istar(2),interval=1,burnin=0)
mcmc.diagnostics(fit)                                   # Poor convergence

# Will restoring thinning/burn-in help?  (Yes, it will.)
fit<-ergm(emon[[1]]~edges+istar(2))
mcmc.diagnostics(fit)                                   # Much better!
mcmc.diagnostics(fit,center=FALSE)                      # Same, w/out centering
summary(emon[[1]]~edges+istar(2))                       # Compare vs. observed
plot(as.matrix(fit$sample))                             # Can plot directly
abline(v=0,h=0,col=2,lty=2)                             # y_obs is at origin

# For more information....
#
#-Goodness-of-Fit/Adequacy Revisited--------------------------------------------
#
# Our edges/instar model mixes OK, so let's see how good it is
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
fit.gof                                                 # Examine p-values
par(mfrow=c(2,2))
plot(fit.gof)                                           # Distribution plots


##################
## Example: Newcomb Fraternity
##################

# newfrat$newfrat.dich contains the dichotomized rank-order preference data from Newcomb's
# fraternity study; in this case, an edge from i to j indicates that j was
# in i's top 50% of all rankings.  A side effect of this is to fix outdegree
# at 8 -- let's see what happens when we ignore this:
data(newfrat)

fit<-ergm(newfrat$newfrat.dich[[15]]~edges+mutual+ttriple+istar(2))
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit.gof)                                               # Outdegree problem!
summary(fit)

# Clearly, something is wrong -- our outdegree prediction is off.  Let's try
# fitting the same model with fixed outdegrees (note that this also fixes
# the edges term).
fit2<-ergm(newfrat$newfrat.dich[[15]]~mutual+ttriple+istar(2),constraints=~odegreedist,
           interval=5e2,MCMCsamplesize=3e4)
fit2.gof<-gof(fit2,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit2.gof)
summary(fit2)

######################
## Example: Sampson
######################
data(sampson)

fit<-ergm(sampson[[7]]~edges+mutual+ttriple+istar(2))
fit<-ergm(sampson[[7]]~edges+mutual+ttriple+istar(2)+ostar(2))
mcmc.diagnostics(fit)                                   # Usually unstable
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit.gof)
fit<-ergm(sampson[[7]]~edges+mutual+ttriple+istar(2)+ostar(2),
          constraints=~bd(maxout=4))
par(mfrow=c(2,2))
plot(fit.gof)

###############
## Example: Coleman
###############

data(coleman)
colespr<-network(coleman[2,,])
fit<-ergm(colespr~edges+mutual+ttriple+istar(2)+ostar(2))
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit.gof)

fit<-ergm(colespr~edges+mutual+ttriple+gwidegree(1)+gwodegree(1)+gwesp(1))


###################
## Example: Lawyers
###################
data(lazega)


efit <- ergm(lazega[[1]]~edges
             + match("office")
             + nodecov("seniority"),
             MPLEonly=TRUE)

# Condition on the outdegrees
#
efit <- ergm(lazega[[1]]~edges
             + match("office")
             + match("practice") + mutual)

summary(efit)


###################
## Example Children
###################
data(bott)

# Edge is when one child irritated another child
gplot(bott[[4]])

bott[[4]]
par(mfrow=c(1,2)) # Setup a 2 panel plot (for later)
gplot(bott[[4]], main="Bott-Preschool", cex.main=0.8)
summary(bott[[4]]~edges) # Look at the $g(y)$ statistic for this model
bottmodel.01 <- ergm(bott[[4]]~edges) # Estimate the model 
summary(bottmodel.01) # The fitted model object

summary(bott[[4]]~edges+triangle)
bottmodel.02 <- ergm(bott[[4]]~edges + triangle)
summary(bottmodel.02)
class(bottmodel.02)

bott[[4]]
age <- bott[[4]] %v% "age.month"
summary(age)
gplot(bott[[4]], main="Bott-Preschool", cex.main=0.8, vertex.cex=age/24)
summary(bott[[4]]~edges+nodecov('age.month'))
bottmodel.03 <- ergm(bott[[4]]~edges+nodecov('age.month'))
summary(bottmodel.03)

bottmodel.04 <- ergm(bott[[4]]~edges+mutual)
summary(bottmodel.04)

adj.01<-as.sociomatrix(bott[[1]])
adj.01

# Test the irritation network also using edges from the talking network
bottmodel.05 <- ergm(bott[[4]]~edges+edgecov(bott[[1]]))

# The probability of an edge in bott[[4]] is 0.17, but if it also appears 
# in the talking network then the probability increases to 0.39

agediff <- abs(outer(bott[[4]]%v%"age.month",bott[[4]]%v%"age.month","-"))
bottmodel.06 <- ergm(bott[[4]]~edges+edgecov(bott[[1]])+edgecov(agediff))
summary(bottmodel.06)

# Adding the age difference between dyadic pairs changes significance level of
# edges but not the edges from the talking network (bott[[1]])

## Testing GOF ------------------------------------------------------------
bottmodel.06.gof <- gof(bottmodel.06~ esp + distance)
bottmodel.06.gof
plot(bottmodel.06.gof)

###################
## Example Monkeys
###################
data(gandg) 
monkey <- gandg$grooming

summary(monkey~edges) # Look at the $g(y)$ statistic for this model
monkey.edges <- ergm(monkey~edges) # Estimate the model 
summary(monkey.edges) # The fitted model object

summary(monkey~edges+triangle) # Look at the g(y) stats for this model
monkey.edgetri <- ergm(monkey~edges+triangle) 
summary(monkey.edgetri)

gender <- monkey %v% 'sex' # %v% references vertex attributes
age <- monkey %v% 'age_grade' # %v% references vertex attributes


plot(monkey, vertex.col=gender, vertex.cex=age, main="Monkeys", cex.main=0.8)
summary(flomarriage~edges+nodecov('wealth')) # observed statistics for the model
monkey.gender <- ergm(monkey~edges+triangle+nodecov('sex'))
summary(monkey.gender)

monkey.age <- ergm(monkey~edges+triangle+nodecov('sex')+nodecov('age_grade'))
summary(monkey.age)

monkey.match <- ergm(monkey~edges+triangle+nodecov('age_grade')+nodematch('sex',diff=T))
summary(monkey.match)

mixingmatrix(monkey,'sex')

monkey.mutual <- ergm(monkey~edges+triangle+nodecov('age_grade')+nodematch('sex',diff=T)+mutual)
summary(monkey.mutual)

mcmc.diagnostics(monkey.mutual)
gof(monkey.mutual)

m <- ergm(monkey~edges+nodecov('age_grade'))
summary(m)

gof <- gof(m)
plot(gof)

m <- ergm(monkey~edges+nodeicov('age_grade'))
summary(m)

m <- ergm(monkey~edges+nodeocov('age_grade'))
summary(m)

m <- ergm(monkey~edges+sender)
summary(m)
