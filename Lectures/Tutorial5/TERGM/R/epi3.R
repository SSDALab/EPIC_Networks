library(ergm)
library(sna)
Vmax<-1000
base<-1000
#state<-c(rep("alive",base),rep(NA,Vmax-base))
p<-.05

size<-base
net<-network(size,directed=FALSE)
net%v%"gender"<-rbinom(size,1,.5)
net%v%"diffstate"<-rbinom(size,1,p)
sim <- simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-3,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))
set.edge.attribute(sim,"diffrate",.02)
sim<-diffusion(sim)

time<-75
save<-vector("list",time)
for(i in 1:time){
state<-as.numeric(sim%v%"diffdepth"<=1)
state[is.na(state)]<-0
sim%v%"diffstate"<-state
set.edge.attribute(sim,"diffrate",.02)
sim<-diffusion(sim)
state<-as.numeric(sim%v%"diffdepth"<=1)
state[is.na(state)]<-0
death<-rbinom(1:network.size(sim),1,.01+state*.2)
delete.vertices(sim, which(death==1))
sim<-simulate(sim ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+cycle(9)+cycle(10)+nodematch("gender")+edgecov(sim[,]), coef=c(-5,3.5,-.5,-1000,-100,-100,-100,-100,-100,-100,-100,-10000000,2),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))	
save[[i]]<-sim
print(i)
}


i<-1
pdf(paste(i,".pdf",sep=""))
state<-as.numeric(save[[i]]%v%"diffdepth"<=1)
state[is.na(state)]<-0
coord<-plot(save[[i]],vertex.col=state,jitter=FALSE,vertex.cex=.7)
dev.off()

for(i in 2:time){
pdf(paste(i,".pdf",sep=""))
state<-as.numeric(save[[i]]%v%"diffdepth"<=1)
state[is.na(state)]<-0
coord2<-coord[(save[[1]]%v%"vertex.names")%in%(save[[i]]%v%"vertex.names"),]
plot(save[[i]],coord=coord2,vertex.col=state,edge.col=rgb(0,0,0,.1),vertex.cex=.7,jitter=FALSE)
dev.off()
print(i)
}


#for(i in 1:30){
#	pdf(paste(i,".pdf",sep=""))
#	plot(save[[i]],vertex.col="HIV")
#	dev.off()
#	print(i)
#}
library(diffusion)
i<-1
pdf(paste(i,".pdf",sep=""))
coord<-plot(save[[i]],vertex.col="HIV",jitter=FALSE,vertex.cex=.7)
dev.off()
temp<-save[[i]]
temp%v%"diffstate"<-temp%v%"HIV"
set.edge.attribute(temp,"diffrate",.0001)
temp<-diffusion(temp)

for(i in 2:time){
state<-as.numeric(temp%v%"diffdepth"<=1)
state[is.na(state)]<-0
temp<-network(save[[i]][,]+temp[,],directed=FALSE)
temp%v%"diffstate"<-state
set.edge.attribute(temp,"diffrate",.0001)
temp<-diffusion(temp)
state<-as.numeric(temp%v%"diffdepth"<=1)
state[is.na(state)]<-0
pdf(paste(i,".pdf",sep=""))
plot(temp,coord=coord,vertex.col=state,edge.col=rgb(0,0,0,.1),vertex.cex=.7)
dev.off()
print(i)
}






