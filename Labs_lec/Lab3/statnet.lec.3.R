################################################################################
#
#  Network Methods Lecture 3 - R/Statnet Component
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

#Load data for today
load("nmlec3.Rdata")

#
#Basic centrality indices: degree, betweenness, and closeness-------------------
#
# We begin with the simplest case: degree
degree(mids_1993)                                        # Default: total degree
ideg <- degree(mids_1993, cmode="indegree")              # Indegree for MIDs
odeg <- degree(mids_1993, cmode="outdegree")             # Outdegree for MIDs
all(degree(mids_1993) == ideg+odeg)                      # In + out = total?

# Once centrality scores are computed, we can handle them using standard R 
# methods:
plot(ideg, odeg, type="n", xlab="Incoming MIDs", ylab="Outgoing MIDs")
abline(0, 1, lty=3)
text(jitter(ideg), jitter(odeg), network.vertex.names(contig_1993), cex=0.75, 
    col=2)   #Plot index by odeg

#Plot simple histograms of the degree distribution:
par(mfrow=c(2,2))                                       # Set up a 2x2 display
hist(ideg, xlab="Indegree", main="Indegree Distribution", prob=TRUE)
hist(odeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
hist(ideg+odeg, xlab="Total Degree", main="Total Degree Distribution", 
    prob=TRUE)
par(mfrow=c(1,1))                                       # Restore display

# Centrality scores can also be used with other sna routines, e.g., gplot
gplot(mids_1993, vertex.cex=(ideg+odeg)^0.5/2, vertex.sides=50, 
    boxed.labels=FALSE,label.cex=0.4, 
    vertex.col=rgb(odeg/max(odeg),0,ideg/max(ideg)), 
    label=network.vertex.names(mids_1993))

# Betweenness and closeness are also popular measures
bet <- betweenness(contig_1993, gmode="graph")       # Geographic betweenness
bet
gplot(contig_1993, vertex.cex=sqrt(bet)/25, gmode="graph")   # Use w/gplot
clo <- closeness(contig_1993)                        # Geographic closeness
clo                                                  # A large world after all?

# Can use sna routines to explore alternatives to the common measures....
closeness2 <- function(x){            # Create an alternate closeness function!
    geo <- 1/geodist(x)$gdist         # Get the matrix of 1/geodesic distance
    diag(geo) <- 0                    # Define self-ties as 0
    apply(geo, 1, sum)                # Return sum(1/geodist) for each vertex
}
clo2 <- closeness2(contig_1993)       # Use our new function on contiguity data
hist(clo2, xlab="Alt. Closeness", prob=TRUE)    # Much better behaved!
cor(clo2, bet)                                  # Correlate with betweenness
plot(clo2, bet)                            # Plot the bivariate relationship

#For more information....
?betweenness
?bonpow
?closeness
?degree
?evcent
?graphcent
?infocent
?prestige
?stresscent

#
#Simple hypothesis tests for NLIs----------------------------------------------
#
library(network)                               #Load network if needed
data(emon)                                     #Load Drabek et al. data

#Extract ties from the Cheyenne EMON communicating at least "every few hours"
g<-as.sociomatrix(emon[[1]],"Frequency")       #Need to get the frequency info
g<-symmetrize((g>0)&(g<4))                     #Note the reverse coding!

#Get some potential covariates
drs<-emon[[1]]%v%"Decision.Rank.Score"         #Get decision rank (see man page)
crs<-emon[[1]]%v%"Command.Rank.Score"          #Get command rank

#Calculate some basic centrality measures
deg<-degree(g,gmode="graph")
bet<-betweenness(g,gmode="graph")
clo<-closeness(g,gmode="graph")

#Raw correlations
cor(cbind(deg,bet,clo),cbind(drs,crs))

#Classical tests (using asymptotic t distribution)
cor.test(deg,drs)
cor.test(bet,drs)
cor.test(clo,drs)

#Permutation tests
perm.cor.test<-function(x,y,niter=5000){  #Define a simple test function
  c.obs<-cor(x,y,use="complete.obs")
  c.rep<-vector()
  for(i in 1:niter)
    c.rep[i]<-cor(x,sample(y),use="complete.obs")
  cat("Vector Permutation Test:\n\tObserved correlation: ",c.obs,"\tReplicate quantiles (niter=",niter,")\n",sep="")
  cat("\t\tPr(rho>=obs):",mean(c.rep>=c.obs),"\n")
  cat("\t\tPr(rho<=obs):",mean(c.rep<=c.obs),"\n")
  cat("\t\tPr(|rho|>=|obs|):",mean(abs(c.rep)>=abs(c.obs)),"\n")
  invisible(list(obs=c.obs,rep=c.rep))
}
perm.cor.test(deg,drs)                     #Non-parametric tests of correlation
perm.cor.test(bet,drs)
perm.cor.test(clo,drs)

#For more information....
?emon
?cor.test
?t.test
?sample

#
#Using NLIs as regression covariates--------------------------------------------
#
pstaff<-emon[[1]]%v%"Paid.Staff"                     # Get more EMON covariates
vstaff<-emon[[1]]%v%"Volunteer.Staff"
govt<-((emon[[1]]%v%"Sponsorship")!="Private")

#Very simple model: decision rank is linear in size, degree, and govt status
mod<-lm(drs~deg+pstaff+vstaff+govt)
summary(mod)
anova(mod)                                            #Some useful lm tools
AIC(mod)

#Does total size change the picture?
mod2<-lm(drs~deg+I(pstaff+vstaff)+govt)                #Pre-add sizes
summary(mod2)

#Try with alternative measures....
mod3<-lm(drs~bet+pstaff+vstaff+govt)                   #Betweenness
summary(mod3)
mod4<-lm(drs~clo+pstaff+vstaff+govt)                   #Closeness
summary(mod4)
AIC(mod,mod3,mod4)                                     #Closeness wins!

#For more information....
?lm
?anova
?AIC
