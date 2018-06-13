################################################################################
#
#  Network Methods Lecture 19 - R/Statnet Component
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

# Load today's data
load("nmlec19.Rdata")
#
#-Using constraints-------------------------------------------------------------
#
# newfrat$newfrat.dich contains the dichotomized rank-order preference data from Newcomb's
# fraternity study; in this case, an edge from i to j indicates that j was
# in i's top 50% of all rankings.  A side effect of this is to fix outdegree
# at 8 -- let's see what happens when we ignore this:
fit<-ergm(newfrat$newfrat.dich[[15]]~edges+mutual+ttriple+istar(2))
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit.gof)                                               # Outdegree problem!
summary(fit)

# Clearly, something is wrong -- our outdegree prediction is off.  Let's try
# fitting the same model with fixed outdegrees (note that this also fixes
# the edges term).
fit2<-ergm(newfrat$newfrat.dich[[15]]~mutual+ttriple+istar(2),constraints=~outdegreedist,
  interval=5e2,MCMCsamplesize=3e4)
fit2.gof<-gof(fit2,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit2.gof)
summary(fit2)

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


data(coleman)
colespr<-network(coleman[2,,])
fit<-ergm(colespr~edges+mutual+ttriple+istar(2)+ostar(2))
fit.gof<-gof(fit,GOF=~idegree+odegree+espartners+distance)
par(mfrow=c(2,2))
plot(fit.gof)

fit<-ergm(colespr~edges+mutual+ttriple+gwidegree(1)+gwodegree(1)+gwesp(1))

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
?ergm
?mcmc.diagnostics
?summary.emon
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

# For more information....
?gof.ergm
?"ergm-terms"
