################################################################################
#
#  Network Methods Lecture 18 - R/Statnet Component
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
#-Simulation Diagnostics--------------------------------------------------------
#
# Load the EMON data
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
