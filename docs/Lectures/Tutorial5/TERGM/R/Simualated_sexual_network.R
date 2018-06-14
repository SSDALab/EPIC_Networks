require(networkDynamic)
require(network)
require(ndtv)
require(sna)
require(animation)
require(katrina)
require(diffusion)
require(ergm)
#data(katrina.combined)
#data(katrina.bydate)

###########
## Simulation
###########

p<-.35
time<-15
r<-1
base<-200

size<-base
net<-network(matrix(0,nc=size,nr=size),directed=FALSE)
net%v%"gender"<-rbinom(size,1,.5)

sim <- simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))
sim<-simulate(sim ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+nodematch("gender")+edgecov(sim[,]), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-100,-10000000,5),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))	
set.edge.attribute(sim,"diffrate",r)
sim%v%"diffstate"<-rbinom(size,1,p)
sim<-diffusion(sim)

HIV<-as.numeric(sim%v%"diffdepth"<=1)	
HIV[is.na(HIV)]<-0
sim%v%"HIV"<-HIV


save<-vector("list",time)
save[[1]]<-sim


for(i in 2:time){ #.05
death<-rbinom(network.size(sim),1,.05+.6*(sim%v%"HIV"))
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

#############################
##### Turn into networkDynamic object
#############################

mapen<-function(net,e){apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})}

buildBase<-function(nl){
nam<-unique(unlist(lapply(nl,function(x){x%v%"vertex.names"})))	
base<-network(matrix(0,nc=length(nam),nr=length(nam)),directed=FALSE)
base%v%"vertex.names"<-nam
for(i in 1:length(nl)){
net<-as.matrix(nl[[i]],matrix.type="edgelist")
m1<-match(attr(net,"vnames")[net[,1]],base%v%"vertex.names")
m2<-match(attr(net,"vnames")[net[,2]],base%v%"vertex.names")
base<-add.edges(base,m1,m2)
}
gen<-lapply(nl,function(x){cbind(x%v%"vertex.names",x%v%"gender")})
ogen<-vector()
for(i in 1:length(gen)){ogen<-rbind(ogen,gen[[i]])}
ugen<-unique(ogen)
m<-match(ugen[,1],base%v%"vertex.names")
base%v%"gender"<-as.numeric(ugen[m,2])
base
}

buildNetDyn<-function(nl,base=NULL){
	
base<-buildBase(nl)
t<-network.copy(base)
t%v%"HIV"<-rep(0,network.size(t))

for(i in 1:length(nl)){
net<-as.matrix(nl[[i]],matrix.type="edgelist")
m1<-match(attr(net,"vnames")[net[,1]],base%v%"vertex.names")
m2<-match(attr(net,"vnames")[net[,2]],base%v%"vertex.names")
pl<-cbind(m1,m2)
map<-mapen(t,pl)

for(k in 1:length(map)){
for(j in 1:length(map[[k]])){
t<-activate.edges(t,at=i,e=map[[k]][j])
#
}
}
t<-activate.vertices(t,at=i,v=which((base%v%"vertex.names")%in%(nl[[i]]%v%"vertex.names")))
print(i)
}


#attr<-lapply(nl,function(x){
#data.frame(HIV=x%v%"HIV",nam=x%v%"vertex.names",stringsAsFactors=FALSE)
#})

#battr<-data.frame(HIV=matrix(0,nr=network.size(t),nc=max(t%v%"active")),nam=t%v%"vertex.names",stringsAsFactors=FALSE)

#for(i in 1:length(attr)){
#m<-match(as.character(attr[[i]]$nam),battr$nam)
#battr[m[attr[[i]]$HIV==1],i]<-1
#}


#for(i in 1:length(attr)){
#t<-activate.vertex.attribute(t,"HIV",value=battr[,i],at=i)
#}



class(t) <- c("networkDynamic", class(t))
t
}

dn<-buildNetDyn(save)

slice.par<-list(start=1,end=time,interval=1, aggregate.dur=2,rule="any")
windsurfers <-compute.animation(dn,slice.par=slice.par,animation.mode="MDSJ",default.dist=3)


render.animation(windsurfers,render.par=list(tween.frames=25,show.time=F),vertex.col="gender",edge.col="darkgray",displaylabels=F,label.cex=.6,label.col="blue")


saveVideo(ani.replay(),video.name="diseaseModel.mp4", other.opts="-b 1000k")







