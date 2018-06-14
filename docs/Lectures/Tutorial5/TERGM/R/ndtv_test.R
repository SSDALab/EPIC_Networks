library(ndtv)
#### maps edges to mel index
mapen<-function(net,e){apply(e,1,function(y){get.edgeIDs(net,v=y[1],alter=y[2])})}


### Builds base network (i.e., a network that has all edges ever seen)
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
base
}

#### Adds in the temporal component starting at 1 (i.e., first network in list is time point 1)
buildNetDyn<-function(nl){
t<-buildBase(nl)

for(i in 1:length(nl)){
net<-as.matrix(nl[[i]],matrix.type="edgelist")
m1<-match(attr(net,"vnames")[net[,1]],base%v%"vertex.names")
m2<-match(attr(net,"vnames")[net[,2]],base%v%"vertex.names")
pl<-cbind(m1,m2)
map<-mapen(t,pl)

for(k in 1:length(map)){
for(j in 1:length(map[[k]])){
activate.edges(t,at=i,e=map[[k]][j])
#
}
}
#activate.vertices(t,onset=i,terminus=i,v=which((base%v%"vertex.names")%in%(nl[[i]]%v%"vertex.names")))
print(i)
}

class(t) <- c("networkDynamic", class(t))
t
}

dn<-buildNetDyn(save[1:3])

render.par=list(tween.frames=10,show.time=T, show.stats="~edges")

render.animation(dn,render.par=render.par,edge.col="darkgray",displaylabels=F)

saveVideo(ani.replay(),video.name="nd.mp4", +	other.opts="-b 5000k",clean=TRUE)



















