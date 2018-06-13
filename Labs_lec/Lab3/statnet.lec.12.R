################################################################################
#
#  Network Methods Lecture 12 - R/Statnet Component
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
#-Graph correlation and bivariate QAP-------------------------------------------
#
# Let's load the Florentine families data
data(florentine)
plot(flobusiness)                                 # Examine business ties
plot(flomarriage)                                 # Examine marriage ties

# Could the two be related?  Let's try a graph correlation
gcor(flobusiness,flomarriage)

# To test the correlation, we can use the qaptest routine
qt<-qaptest(list(flobusiness,flomarriage),gcor,g1=1,g2=2)
summary(qt)                                       # Examine the results
plot(qt)                                          # Plot the QAP distribution

# Testing against covariate effects
wealth<-sapply(flomarriage%v%"wealth",rep,network.size(flomarriage))
wealthdiff<-abs(outer(flomarriage%v%"wealth",flomarriage%v%"wealth","-"))
qt1<-qaptest(list(flomarriage,wealth),gcor,g1=1,g2=2)
qt2<-qaptest(list(flomarriage,wealthdiff),gcor,g1=1,g2=2)
summary(qt1)                           # Do wealthy families have more ties?
summary(qt2)                           # Is there a wealth difference effect?

# For more information....
?qaptest
?gcor
?outer
?sapply
?rep
#
#-Network regression------------------------------------------------------------
#
# We begin by preparing the response variable.  We will use the Cheyenne
# EMON in valued form, but need to recode the frequency data
data(emon)
Y<-as.sociomatrix(emon[[1]], "Frequency")       # Extract frequencies
Y[Y>0]<-5-Y[Y>0]                                # Now, higher -> more frequent

# Extract some vertex attributes
crk<-emon[[1]]%v% "Command.Rank.Score"          # Command rank
spon<-emon[[1]]%v%"Sponsorship"                 # Type of organization

# Form some predictor matrices (X variables)
Xcr<-sapply(crk,rep,length(crk))            # Column effects for command rank
Xcr
Xsp<-outer(spon,spon,"!=")                  # Dyadic effect for type difference
Xsp

# Fit the model (takes a while to perform QAP test)
cmfit<-netlm(Y,list(Xcr,Xsp))
summary(cmfit)                              # Examine the results

# For more information....
?outer
?netlm
