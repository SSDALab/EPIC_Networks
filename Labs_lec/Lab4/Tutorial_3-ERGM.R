##########################################################################
#	R & statnet Tutorial for EPIC 2017
#	author: jimi adams
#   Modified by Zack Almquist
#	last updated: 2017-06-03
#   modified on 2018-06-12
#
#	This is just the code from the full Tutorial distributed as a .pdf
##########################################################################

##########################################################################
#	0. 	Setting up your R environment
##########################################################################
#setwd("YOUR WORKING Directory") 	# Path for ALL files w/location unspecified
#install.packages("statnet")			# The first time you install a package
library(statnet)						# Attach the library to your session
#install.packages("coda")				# The first time you install a package
library(coda)
#install.packages("latticeExtra")		# The first time you install a package
library(latticeExtra)

## Requires data from Tutorial One
## if windows this might be data/data
load("data/Tutorial.Rdata")
ls()

##########################################################################
#	1.2 	An ERGM walkthrough.
#			Adapted from Tutorial by the statnet team:
#			J. of Statistical Software 2008;24(3)
##########################################################################
data('faux.mesa.high')    		# Load one of the included datasets (undirected data)
plot.network(faux.mesa.high)     	# Plot the network (for prettier graphs, see 24(9) or Appendix of JSS 24(3))
summary(faux.mesa.high)   		# A few summary statistics

model1 <- ergm(faux.mesa.high ~ edges)
summary(model1)

model2 <- ergm(faux.mesa.high ~ edges + degree(1))
summary(model2)

#  Sex homophily - use the nodematch term
model3 <- ergm(faux.mesa.high ~ edges + nodematch("Sex"))
summary(model3)
exp(model3$coef[2])

#  Grade homophily (homophily coded same/different )
model3a <- ergm(faux.mesa.high ~ edges + nodematch("Grade"))
summary(model3a)
exp(model3a$coef[2])

#  Grade homophily (homophily coded as absolute difference)
model3b <- ergm(faux.mesa.high ~ edges + absdiff("Grade"))
summary(model3b)
exp(model3b$coef[2])

 #  Grade homophily (separate attraction terms for each grade)
model3c <- ergm(faux.mesa.high ~ edges + nodematch("Grade",diff=TRUE))
summary(model3c)

#  Race homophily (separate attraction terms for each race)
model3d <- ergm(faux.mesa.high ~ edges + nodematch("Race"))
table(get.vertex.attribute(faux.mesa.high,'Race'))    # Frequency table of race
summary(model3d)

#  Grade, sex, and race homophily
model3e <- ergm(faux.mesa.high ~ edges + nodematch("Grade") + nodematch("Sex") + nodematch("Race",diff=TRUE))
summary(model3e)

#  Consider whether tendency to form ties varies by individual attributes: sex
model3f <- ergm(faux.mesa.high ~ edges + nodefactor("Sex"))
summary(model3f)
exp(model3f$coef[2])

#  Consider whether tendency to form ties varies by individual: grade
model3g <- ergm(faux.mesa.high ~ edges + nodefactor("Grade"))
summary(model3g)

#  Calculate degree distribution for each grade
summary(faux.mesa.high ~ degree(0:8,"Grade"))
#  Don't treat grade as categorical - use nodecov
model3g1 <- ergm(faux.mesa.high ~ edges + nodecov("Grade"))
summary(model3g1)

#  Consider whether tendency to form ties varies by individual: race
model3h <- ergm(faux.mesa.high ~ edges + nodefactor("Race"))
summary(model3h)

#  Re-estimate model with the largest group (Hispanics=2) as the reference category
model3h1 <- ergm(faux.mesa.high ~ edges + nodefactor("Race", base=2))
summary(model3h1)
exp(model3h1$coef)

model3i <- ergm(faux.mesa.high ~ edges + nodematch("Race", diff = TRUE) + nodefactor("Sex"))
summary(model3i)

model4 <- ergm(faux.mesa.high ~ edges + nodematch("Grade") + gwesp(0.5, fixed = TRUE), verbose = TRUE, control=control.ergm(seed=467))

summary(model4)
model4b <- ergm(faux.mesa.high ~ edges + nodematch("Grade") + gwesp(0.5, fixed=TRUE) + gwdsp(0.5, fixed=TRUE), verbose=FALSE, control=control.ergm(seed=467))
summary(model4b)


##########################################################################
#	1.2.a 	An ERGM walkthrough.
# 			Let's take a look at a degenerate model first to see why we wanted to jump to the GWESP term.
##########################################################################
data('faux.magnolia.high')
fmh <- faux.magnolia.high
plot(fmh, vertex.cex=.5)
model5a <- ergm(fmh ~edges+triangle, control=control.ergm(seed=1))
#mcmc.diagnostics(model5a, center=F)
model5a <- ergm(fmh ~edges+triangle, control=control.ergm(seed=1,MCMLE.maxit=2))
mcmc.diagnostics(model5a, center=F)
model5b <- ergm(fmh ~edges+gwesp(0.5,fixed=TRUE), control =  control.ergm(seed=1))
mcmc.diagnostics(model5b, center=F)
model5c <- ergm(fmh ~edges+gwesp(0.25,fixed=TRUE), control = control.ergm(seed=1), verbose=T)
mcmc.diagnostics(model5c, center=F)
model5d <- ergm(fmh ~edges+gwesp(0.25,fixed=TRUE)+nodematch('Grade')+ nodematch('Race')+nodematch('Sex'), control=control.ergm(seed=1,MCMC.samplesize=50000,MCMC.interval=1000), verbose=F)
mcmc.diagnostics(model5d, center=F)
summary(model5d)
par(mfrow=c(1,3))
model5d.gof<-gof(model5d)
summary(model5d.gof) 				
plot(model5d.gof)
par(mfrow=c(1,1))

##########################################################################
#	1.3 	Code for the Grey's Anatomy Example
#			Adapted from Benjamin Lind
#			http://badhessian.org/2012/09/lessons-on-exponential-random-graph-modeling-from-greys-anatomy-hook-ups/
##########################################################################
la <- plot(greys)		#fix plot coordinates, so they don't jump around if we replot later
plot(greys, coord=la, vertex.col=get.vertex.attribute(greys, "nrace"), 		# Numeric converstion of race variable
			label=get.vertex.attribute(greys, "name"), label.cex=.75, 		# Names
			vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 	# circles=women, squares=men
			vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	# size proportional to age

##########################################################################
# 	1.3.a	These are all of the models presented / mentioned in class, without comment. 
#			See section 1.4.b if you'd like to walk through a more detailed model fitting example with these data.
#			Some of the models are repeated in section a/b. Those in b will take longer to run.
##########################################################################
# Running the Models
gr.bern <- ergm(greys~edges) 
gr.bern
summary(gr.bern)
gr.nisol<- ergm(greys~edges+degree(1))
gr.nisol.het <- ergm(greys~edges+degree(1)+nodematch("sex"))
gr.nisol.het.r <- ergm(greys~edges+degree(1)+nodematch("sex")+nodefactor("race"))
gr.nisol.het.rh <- ergm(greys~edges+degree(1)+nodematch("sex")+ nodematch("race"))
gr.nisol.het.aged <- ergm(greys~edges+degree(1)+nodematch("sex")+ absdiff("birthyear"))

# Summarizing the Models
summary(gr.bern)
summary(gr.nisol)
summary(gr.nisol.het)
summary(gr.nisol.het.r)
summary(gr.nisol.het.rh)
summary(gr.nisol.het.aged)

# A few not used
#gr.2star<- ergm(greys~edges+degree(c(1,2)))
#gr.3star<- ergm(greys~edges+degree(c(1,2,3)))
#gr.4star<- ergm(greys~edges+degree(c(1,2,3,4)))
#gr.nisol.tri<- ergm(greys~edges+degree(1)+triangle)
#summary(gr.2star)
#summary(gr.3star)
#summary(gr.4star)
#summary(gr.nisol.tri)

# A few simulations & plots
gr.bern.sim <- simulate(gr.bern, nsim=10)
summary(gr.bern.sim)
plot(gr.bern.sim[[2]])
gr.nisol.sim <- simulate(gr.nisol, nsim=10)
summary(gr.nisol.sim)
plot(gr.nisol.sim[[1]])
gr.nisol.het.sim <- simulate(gr.nisol.het, nsim=10)
summary(gr.nisol.het.sim)
plot(gr.nisol.het.sim[[1]], vertex.cex=1.5, vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4))
gr.nisol.het.aged.sim <- simulate(gr.nisol.het.aged, nsim=10)
summary(gr.nisol.het.aged.sim)
plot(gr.nisol.het.aged.sim[[1]], vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))

# This produces the Goodness of Fit plots for the models presented in class, and the summaries of the same
par(mfrow=c(1,3))
gr.bern.gof<-gof(gr.bern)
summary(gr.bern.gof) 				
plot(gr.bern.gof)
gr.nisol.gof<-gof(gr.nisol)
summary(gr.nisol.gof) 				
plot(gr.nisol.gof)
gr.nisol.het.gof<-gof(gr.nisol.het)
summary(gr.nisol.het.gof) 				
plot(gr.nisol.het.gof)
gr.nisol.het.aged.gof<-gof(gr.nisol.het.aged)
summary(gr.nisol.het.aged.gof) 				
plot(gr.nisol.het.aged.gof)

# Taking a look at the MCMCMLE diagnostics
mcmc.diagnostics(gr.bern)
mcmc.diagnostics(gr.nisol)
mcmc.diagnostics(gr.nisol.het)
mcmc.diagnostics(gr.nisol.het.aged)


##########################################################################
# 	1.3.b	A slightly more detailed/systematic model building/fitting exploration.
##########################################################################
# Bernoulli
gr.bern <- ergm(greys~edges) 				# Bernoulli 
summary(gr.bern)
plot(simulate(gr.bern))			# What does a simulated network of that model look like?
gr.bern.gof <- gof(gr.bern)					# Estimating Goodness of Fit with default set of diagnostics
summary(gr.bern.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.bern.gof)
par(mfrow=c(1,1))


# Sex heterophily
gr.het <- ergm(greys~edges+nodematch("sex")) 	# Estimate the model, w/ sex-match estimation
plot(simulate(gr.het), vertex.cex=1.5,			# Take a look
		vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4))						
gr.het.gof <- gof(gr.het)						# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het.gof) 							# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het.gof)
par(mfrow=c(1,1))
summary(gr.het) 								# Summarize the model

# No Isolates
gr.het_noisol <- ergm(greys~edges+nodematch("sex")+degree(1)) 	# Estimate the model, adding isolation constraint
summary(gr.het_noisol)
plot(simulate(gr.het_noisol), vertex.cex=1.5,	# Take a look
		vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4))						
gr.het_noisol.gof<-gof(gr.het_noisol)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol)

# Respecifying the same model, with some additional details fed into the MCMCMLE
gr.het_noisol<-ergm(greys~edges+nodematch("sex")+degree(1), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
summary(gr.het_noisol)							# no real change here
mcmc.diagnostics(gr.het_noisol)					# some here

# Age Assortativity
gr.het_noisol_age<-ergm(greys~edges+nodematch("sex")+degree(1)+absdiff("birthyear"), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_age), vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 
		vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	
gr.het_noisol_age.gof<-gof(gr.het_noisol_age)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol_age.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol_age.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_age) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol_age)

# Race Homophily
gr.het_noisol_age_race<-ergm(greys~edges+nodematch("sex")+degree(1)+absdiff("birthyear")+nodematch("race"), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_age_race), vertex.col=get.vertex.attribute(greys, "nrace"), 
		vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 
		vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	
gr.het_noisol_age_race.gof<-gof(gr.het_noisol_age_race)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol_age_race.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol_age_race.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_age_race) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol_age_race)

# Race Homophily, allowing it to  differs by race
gr.het_noisol_age_race1<-ergm(greys~edges+nodematch("sex")+degree(1)+absdiff("birthyear")+nodematch("race", diff=TRUE), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_age_race1), vertex.col=get.vertex.attribute(greys, "nrace"), 
		vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4), 
		vertex.cex=.05*(2013-get.vertex.attribute(greys, "birthyear")))	
gr.het_noisol_age_race1.gof<-gof(gr.het_noisol_age_race1)			# Estimating Goodness of Fit with default set of diagnostics
summary(gr.het_noisol_age_race1.gof) 						# Summarize the goodness of fit
par(mfrow=c(1,3)); plot(gr.het_noisol_age_race1.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_age_race1) 							# Summarize the model
mcmc.diagnostics(gr.het_noisol_age_race1)

# Above, dropping agediff
gr.het_noisol_race<-ergm(greys~edges+nodematch("sex")+degree(1)+nodematch("race"), control=control.ergm(MCMC.burnin=50000, MCMC.interval=5000))
plot(simulate(gr.het_noisol_race), vertex.col=get.vertex.attribute(greys, "nrace"), 
		vertex.sides=(50*(get.vertex.attribute(greys, "sex")=="F")+4))	
gr.het_noisol_race.gof<-gof(gr.het_noisol_race)
summary(gr.het_noisol_race.gof) 				
par(mfrow=c(1,3)); plot(gr.het_noisol_race.gof)
par(mfrow=c(1,1))
summary(gr.het_noisol_race) 							
mcmc.diagnostics(gr.het_noisol_race)