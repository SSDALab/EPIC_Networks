library(ergm)

######################
#### Population Model
#######################
#https://www.cia.gov/library/publications/the-world-factbook/geos/ug.html
#3.582% (2012 est.)
#47.38 births/1,000 population (2012 est.)
#11.54 deaths/1,000 population (July 2012 est.)
#rbinom(1,1,47.38/1000)




#####################
#### Initial Conditions
#####################
initialCond<-function(base,p=.2){
size<-length(base)
net<-network(size,directed=FALSE)
net%v%"gender"<-rbinom(size,1,.5)
net%v%"HIV"<-rbinom(size,1,p)
g.sim <- simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+nodematch("gender"), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-10000000),nsim=1,control=control.simulate(MCMC.burnin=100000, MCMC.interval=1000))	
attr<-data.frame(status=base,gender=net%v%"gender",HIV=net%v%"HIV",stringsAsFactors=FALSE)
list(size=size,attr=attr,net=g.sim)
}

forcast<-function(net){
simulate(net ~ edges+degree(1)+concurrent()+cycle(3)+cycle(4)+cycle(5)+cycle(6)+cycle(7)+cycle(8)+nodematch("gender")+edgecov(net[,]), coef=c(-2,3,-.5,-1000,-100,-100,-100,-100,-100,-10000000,10),nsim=1,control=control.simulate(MCMC.burnin=20000, MCMC.interval=1000))
}

spread<-function(net,sp=.15){
	HIV<-net%v%"HIV"
	for(i in 1:sum(net%v%"HIV"==1)){
	temp<-net[which(net%v%"HIV"==1)[i],]
	#print(temp)
	if(!any(is.na(temp))){
	if(sum(temp)>0){
	HIV[temp==1]<-rbinom(sum(temp),1,sp)
	}
	}
	}
	net%v%"HIV"<-HIV
	net
}

pop<-function(base){
N<-base$attr[,1]
index<-which(rbinom(sum(N=="alive"),1,11.54/1000+(base$net%v%"HIV")*.1)==1)
N[N=="alive"][index]<-"dead"
#*.5*.5*.0323
#babies<-sum(rbinom(sum(N=="alive")*.5*.5,1,47.38/1000))
babies<-length(index)
N<-c(N,rep("alive",babies))
index2<-which(rbinom(sum(N=="alive"),1,0+(base$net%v%"HIV")*.1)==1)
N[N=="alive"][index2]<-"dead"
index<-c(index,index2)
#cat("length",length(index),"babies",babies,"alive",sum(N=="alive"),"\n")
net<-base$net
net<-network.copy(net)
attr<-base$attr
if(length(index)>0){
net<-delete.vertices(net,index)
net<-add.vertices(net,babies)
gender<-net%v%"gender"
gnew<-rbinom(sum(is.na(net%v%"gender")),1,.5)
gender[is.na(gender)]<-gnew
net%v%"gender"<-gender

HIV<-net%v%"HIV"
hnew<-rep(0,sum(is.na(HIV)))
HIV[is.na(HIV)]<-hnew
net%v%"HIV"<-HIV
attr<-data.frame(status=N,gender=c(attr[,2],gnew),HIV=c(attr[,3],hnew),stringsAsFactors=FALSE)
}
net2<-spread(net)
net2<-forcast(net)

list(size=sum(N=="alive"),attr=attr,net=net2)
}


##################
#### Time 1
##################
#plot(g.sim,vertex.col="gender")

##################
#### Time 2
##################


#par(mfrow=c(1,2))
#plot(g.sim,vertex.col="gender")
#plot(g.sim2,vertex.col="gender")

t<-20
N<-200
base<-rep("alive",N)
start<-initialCond(base)
save<-vector("list",t)
net<-start$net
save[[1]]<-pop(start)

for(i in 2:t){
	print(i)
	net<-start$net
	save[[i]]<-pop(save[[i-1]])
}

par(mfrow=c(4,5),mar=c(0, 0, 0, 0) + 0.1)

for(i in 1:19)
plot(save[[i]]$net,vertex.col="HIV",vertex.cex=2)

sapply(save,function(x){x$size})











