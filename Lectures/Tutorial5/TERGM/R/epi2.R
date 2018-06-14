library(ergm)
library(sna)


Vmax<-1000
base<-300
state<-c(rep("alive",base),rep(NA,Vmax-base))
p<-.03

size<-base
net<-network(size,directed=FALSE)
net%v%"gender"<-rbinom(size,1,.5)
net%v%"HIV"<-rbinom(size,1,p)
sim <- simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))
sim<-simulate(sim ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+cycle(9)+cycle(10)+nodematch("gender")+edgecov(sim[,]), coef=c(-3,3.5,-.5,-1000,-100,-100,-100,-100,-100,-100,-100,-10000000,1),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))	

library(diffusion)
library(RColorBrewer)
n<-sim

set.edge.attribute(n,"diffrate",1)
n%v%"diffstate"<-sim%v%"HIV"
n<-diffusion(n)
col<-rep("white",network.size(n))
z<-length(unique(n%v%"diffdepth")[!is.na(unique(n%v%"diffdepth"))])
ct<-brewer.pal(z,"RdYlBu")


pdf("0.pdf")
col<-rep("white",network.size(n))
coord<-plot(n,vertex.col=col)
dev.off()
for(i in 1:length(ct)){
pdf(paste(i,".pdf",sep=""))
col[n%v%"diffdepth"==(i-1)]<-ct[i]
plot(n,vertex.col=col,coord=coord)
legend("bottomleft",legend=c(paste("Time ",0:(length(ct)-1),sep=""),"Not Infected"),fill=c(ct,"white"),bty="n",cex=.7)
dev.off()
}


#save(coord,n,ct,file="diffExample.pdf")
### add tie process
sim2<-simulate(sim ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+cycle(9)+cycle(10)+nodematch("gender")+edgecov(sim[,]), coef=c(-3,3.5,-.5,-1000,-100,-100,-100,-100,-100,-100,-100,-10000000,1),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))	

infected<-n%v%"diffdepth"
infected[!is.na(infected)]<-1
infected[is.na(infected)]<-0




n2<-network(sim2[,]+sim[,],directed=FALSE)
set.edge.attribute(n2,"diffrate",1)
n2%v%"diffstate"<-net%v%"HIV"
n2<-diffusion(n2)
col<-rep("white",network.size(n))
z<-length(unique(n2%v%"diffdepth")[!is.na(unique(n2%v%"diffdepth"))])
ct<-brewer.pal(z,"RdYlBu")


pdf("0.pdf")
col<-rep("white",network.size(n))
coord<-plot(n2,vertex.col=col)
dev.off()
for(i in 1:length(ct)){
pdf(paste(i,".pdf",sep=""))
col[n2%v%"diffdepth"==(i-1)]<-ct[i]
plot(n2,vertex.col=col,coord=coord)
legend("bottomleft",legend=c(paste("Time ",0:(length(ct)-1),sep=""),"Not Infected"),fill=c(ct,"white"),bty="n",cex=.7)
dev.off()
}


##########
### Death
########

library(diffusion)
library(RColorBrewer)
n<-sim
deg<-degree(sim)
index<-which(deg==2)
s<-sample(index,length(index)*.1)
n<-add.vertices(n,1)
n[network.size(n),s]<-1

set.edge.attribute(n,"diffrate",1)
n%v%"diffstate"<-c(sim%v%"HIV",1)
n<-diffusion(n)
col<-rep("white",network.size(n))
z<-length(unique(n%v%"diffdepth")[!is.na(unique(n%v%"diffdepth"))])
ct<-brewer.pal(z,"RdYlBu")


pdf("0.pdf")
col<-rep("white",network.size(n))
coord<-plot(n,vertex.col=col)
dev.off()
for(i in 1:length(ct)){
pdf(paste(i,".pdf",sep=""))
col[n%v%"diffdepth"==(i-1)]<-ct[i]
plot(n,vertex.col=col,coord=coord)
legend("bottomleft",legend=c(paste("Time ",0:(length(ct)-1),sep=""),"Not Infected"),fill=c(ct,"white"),bty="n",cex=.7)
dev.off()
}





attr<-data.frame(status=base,gender=net%v%"gender",HIV=net%v%"HIV",stringsAsFactors=FALSE)