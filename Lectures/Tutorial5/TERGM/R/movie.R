library(ergm)
library(sna)
library(diffusion)

Vmax<-1000
base<-100
state<-c(rep("alive",base),rep(NA,Vmax-base))
p<-.1

size<-base
net<-network(size,directed=FALSE)
net%v%"gender"<-rbinom(size,1,.5)
net%v%"HIV"<-rbinom(size,1,p)
sim <- simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))
sim<-simulate(sim ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+nodematch("gender")+edgecov(sim[,]), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-100,-10000000,5),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))	

time<-60
save<-vector("list",time)
save[[1]]<-sim
for(i in 2:time){
death<-rbinom(network.size(sim),1,.01+.05*(sim%v%"HIV"))
if(sum(death)>0){
	sim<-delete.vertices(sim,which(death==1))
}
new<-rbinom(network.size(sim),1,.02)
if(sum(new)>0){
	sim<-add.vertices(sim,sum(new))
	HIV<-sim%v%"HIV"
	HIV[is.na(HIV)]<-rbinom(sum(is.na(HIV)),1,p)
	sim%v%"HIV"<-HIV
	gender<-sim%v%"gender"
	gender[is.na(gender)]<-rbinom(sum(is.na(gender)),1,.5)
	sim%v%"gender"<-gender
	nam<-sim%v%"vertex.names"
	nam[is.na(nam)]<-paste(i,sample(100:10000,sum(is.na(nam))),sep="")
	sim%v%"vertex.names"<-nam
}

sim<-simulate(sim ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+cycle(9)+nodematch("gender")+edgecov(sim[,]), coef=c(-2.3,2.5,-.55,-1000,-100,-100,-100,-100,-100,-100,-10000000,4),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))

sim%v%"diffstate"<-sim%v%"HIV"
set.edge.attribute(sim,"diffrate",.01)
sim<-diffusion(sim)
update<-as.numeric(sim%v%"diffdepth"<=1)
update[is.na(update)]<-0
sim%v%"HIV"<-update

save[[i]]<-sim
print(i)
}

par(ask=TRUE,mar=c(0, 0, 0, 0) + 0.1)
for(i in 1:time){plot(save[[i]],vertex.col="HIV")}














