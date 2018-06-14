library(ergm)
library(sna)
size<-100
p<-.1
net<-network(size,directed=FALSE)
set.seed(1600)
net%v%"gender"<-rbinom(size,1,.5)
set.seed(1900)
net%v%"HIV"<-rbinom(size,1,p)

set.seed(1982)
net<-simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000),seed=1982)	

set.seed(2010)
sim<-simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+nodematch("gender")+edgecov(net[,]), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-100,-10000000,10),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000),seed=2010)


coord<-plot(sim,vertex.col="gender")

library(diffusion)
n<-sim
set.edge.attribute(sim,"diffrate",1)
n%v%"diffstate"<-sim%v%"HIV"
n<-diffusion(n)

plot(n,vertex.col="diffdepth")
depth<-unique(n%v%"diffdepth")
depth[is.na(depth)]<-10
legend("bottomleft",legend=1:length(depth),col=depth,pch=19)


##############
### Scenerios 
### 
### Fixed Network
###	Deletions 
###	Additions
### Additions and Deletions





















