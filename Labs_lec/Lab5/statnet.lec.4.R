################################################################################
#
#  Network Methods Lecture 4 - R/Statnet Component
#
#  SOC 280 - Analysis of Social Network Data
#  Carter T. Butts, University of Califorina, Irvine
#  Spring Quarter, 2009
#
################################################################################
#
#-A reminder on getting started....---------------------------------------------
#
library(sna)                      # Load the sna library

# Note: today's exercise requires the numDeriv package.  It's normally 
# considered optional, so the system doesn't install it by default.  You can
# be sure that you have the package ready to roll by typing
#
#     install.packages("numDeriv")
#
# before beginning the exercise.
#
#Network autocorrelation models - the generative side---------------------------
#
# Let's begin by constructing some simulated data:
w1<-rgraph(100)                                        #Draw an AR matrix
w2<-rgraph(100)                                        #Draw an MA matrix
x<-matrix(rnorm(100*5),100,5)                          #Draw some covariates
r1<-0.2                                                #Set the model parameters
r2<-0.1
sigma<-0.1
beta<-rnorm(5)
nu<-rnorm(100,0,sigma)                                 #Draw the disturbances

# Assemble y from its components, for several scenarios:
ex<-nu                                         #Draw "raw" errors
yx<-x%*%beta+ex                                #Compute y w/no special effects

ex1<-nu                                        #Draw "raw" errors
yx1<-qr.solve(diag(100)-r1*w1,x%*%beta+ex1)    #Compute y w/AR

ex2<-qr.solve(diag(100)-r2*w2,nu)              #Draw errors w/MA
yx2<-x%*%beta+ex2                              #Compute y w/no special effects

ex12<-qr.solve(diag(100)-r2*w2,nu)             #Draw errors w/MA
yx12<-qr.solve(diag(100)-r1*w1,x%*%beta+ex12)  #Compute y w/AR

#
#Network autocorrelation models - the inferential side--------------------------
#
# Note: some of these models take a while to fit.  This is normal, if 
# regrettable.

# Fit models to the "normal" case - should find that simple is best
fit.x.x<-lnam(yx,x=x)
fit.x.x1<-lnam(yx,x=x,W1=w1)
fit.x.x2<-lnam(yx,x=x,W2=w2)
fit.x.x12<-lnam(yx,x=x,W1=w1,W2=w2)
summary(fit.x.x)
cbind(fit.x.x$beta,beta)                                    #Should be close

# Fit models to the AR case - should find that AR is best
fit.x1.x<-lnam(yx1,x=x)
fit.x1.x1<-lnam(yx1,x=x,W1=w1)
fit.x1.x2<-lnam(yx1,x=x,W2=w2)
fit.x1.x12<-lnam(yx1,x=x,W1=w1,W2=w2)                       #Can be slow
summary(fit.x1.x1)
cbind(fit.x1.x$beta,beta)                                   #Should be scary bad
cbind(fit.x1.x1$beta,beta)                                  #Should be close

# Fit models to the MA case - should find that MA is best
fit.x2.x<-lnam(yx2,x=x)
fit.x2.x1<-lnam(yx2,x=x,W1=w1)
fit.x2.x2<-lnam(yx2,x=x,W2=w2)
fit.x2.x12<-lnam(yx2,x=x,W1=w1,W2=w2)
summary(fit.x2.x2)
cbind(fit.x2.x$beta,beta)                                   #Should be so-so
cbind(fit.x2.x2$beta,beta)                                  #Should be close

# Fit models to the ARMA case - should find that ARMA is best
fit.x12.x<-lnam(yx12,x=x)
fit.x12.x1<-lnam(yx12,x=x,W1=w1)
fit.x12.x2<-lnam(yx12,x=x,W2=w2)
fit.x12.x12<-lnam(yx12,x=x,W1=w1,W2=w2)
summary(fit.x12.x12)
cbind(fit.x12.x$beta,beta)                                    #Should be awful
cbind(fit.x12.x12$beta,beta)                                  #Should be close

# Finally, note that lnam has a plot method
plot(fit.x12.x12)
