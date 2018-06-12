#' ---
#' title: "Lab 1: Descriptives Using STATNET"
#' author: "Zack W Almquist (University of Minnesota)"
#' date: "06/12/2018"
#' output: 
#'   html_document:
#'     toc: true
#' ---
#' 
#' # Necessary R Packages 
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/2,fig.width=12/2----
#library(conflicted)
library(sna)
library(network)
library(networkdata)
library(networkMethods)
#library(igraph)
library(knitr)

#' 
#' # One mode network
#' 
#' AKA Adjacency matrix, sociogram. The matrix plot/spreadsheet form
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/2,fig.width=12/2----
net<-rgraph(5)
colnames(net)<-rownames(net)<-paste("V",1:NROW(net))
kable(as.data.frame(net))

#' 
#' Classic network plot with Fruchterman-Reingold force-directed placement algorithm. Node placement is optimized to show clustering (i.e., more connected nodes are closer).
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/2,fig.width=12/2----
gplot(net)

#' 
#' # Two mode network
#' 
#' **Example: Southern Women** These data were collected by Davis et al. in the 1930s. They represent observed attendance at 14 small social events by 18 Southern women. The result is a person-by-event matrix: cell (i,j) is 1 if person i attended social event j, and 0 otherwise.
#' 
#' **Sociogram**
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/2,fig.width=12/2----
data(davis)
kable(as.sociomatrix(davis))

#' 
#' **Network Plot** with Fruchterman-Reingold force-directed placement algorithm.
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8,fig.width=12----
col<-rep(NA,length(davis%v%"vertex.names"))
col[grep("E[[:digit:]]",(davis%v%"vertex.names"))]<-rep("red")
col[is.na(col)]<-rep("blue")
plot(davis,label="vertex.names",label.cex=.5,vertex.col=col,edge.col=rgb(0,0,0,.5))

#' 
#' # Two mode network to one mode projection
#' 
#' We can take a two mode network and project it down to one mode. For example let's again consider the Southern Women example. Say we want to network of women by women (i.e., the women who are connected by going to the same gathering).
#' We can do that with a matrix operation (in linear algebra terms, $A\times A^T$) in R this is done by `M%*%t(M)` where M is a M is $n\times k$ binary matrix and `t()` is the [transpose function](https://en.wikipedia.org/wiki/Transpose) and `%*%` is the [matrix multiplication operator](https://en.wikipedia.org/wiki/Matrix_multiplication).
#' 
#' In R we can do it as follows,
## ------------------------------------------------------------------------
twomode<-network::as.sociomatrix(davis)
sw_by_sw<-twomode%*%t(twomode)
kable(sw_by_sw)

## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8,fig.width=12----
gplot(sw_by_sw,label=rownames(sw_by_sw),label.cex=.5,edge.col=rgb(0,0,0,.2),gmode="graph")

#' 
#' # Graph/Network Density
#' 
#' * The most basic statistic of a relational dataset is the *mean* or *average*
#' 
#' * **Mean:** The sum of the relational measurements divided by the number of relational measurements.
#' 
#' For a *fully* observed directed relation with $n$ nodes:
#' 
#' - The sum of the relational measurement is $\sum_{i\neq j} y_{ij}$
#' - The number of relational measurments is $n\times (n-1)$
#' - The mean is 
#' 	
#' $$\bar{y} = \frac{\sum_{i\neq j} y_{ij}}{n(n-1)}$$
#' 
#' For a *fully* observed undirected relation with $n$ nodes:
#' 
#' - The sum of the relational measurement is $\sum_{i< j} y_{ij}$
#' - The number of relational measurments is $n\times (n-1)/2$
#' - The mean is 
#' 	
#' $$\bar{y} = \frac{\sum_{i< j} y_{ij}}{n(n-1)/2}$$
#' 
#' Formally, **Density:** is the proportion of edges present in a graph
#' $$= \frac{\textrm{the number of edges}}{\textrm{the maximum possible number of edges}}$$
#' 
#' The number of observed is $|E|$
#' The number of possible edges is
#' 
#' - $n(n-1)$ in a directed graph
#' - $n(n-1)/2$ in an undirected graph
#' 	+ **Derivation** A $n$ by $n$ adjacency matrix (minus its diagonals) has $2*{n \choose 2} = \frac{n!}{2!(n-2)!} = n(n-1)$ cells
#' 
#' $$\delta_d = \frac{|E|}{n(n-1)} \textrm{ , } \delta_u =  \frac{|E|}{n(n-1)/2}$$
#' 
#' 
#' Let $y_{ij}$ be the binary indicator of an eged from $i$ to $j$ and
#' 
#' * $|E|=\sum_{ i < j } y_{ij}$ or an undirected graph
#' * $|E|=\sum_{i \neq j} y_{ij}$ for an undirected graph
#' 
#' 
#' **Densities can be view as**
#' 
#' * Probabilities of the existence of a tie between randomly sampled nodes
#' * Estimates of these probabilities
#' 
#' Let,
#' * $i$ and $j$ be two randomly sampled individuals
#' * Let $\theta$ be the probability that $y_{ij} = 1$
#' $$\Pr(Y_{ij} = 1) = \theta$$
#' 
#' Then,
#' 
#' * $\bar{y} = \theta$ if your nodeset is the entire population of nodes 
#' * $\bar{y} = \hat{\theta}$ if your nodeset is arandom sample of nodes 
#' 
#' # Degree Distribution
#' 
#' Let's consider a network of preschool children. This data was collected by Dr. Bott in 1926 in a preschool in Toronto. Observations were made on each child in turn who was defined as a "focal" individual. Instances in which the focal child (1) talked to another, (2) interfered with another, (3) watched another, (4) imitated another or (5) cooperated with another were tabulated along with the name of the other to whom the social behavior was directed. The result was tabulated in five matrices. Female children are identified with an astrisk on their row and column label.
#' 
#' We will look at the imitation network.
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
data(bott)
imitation<-bott[[4]]
imitation[,]
plot(imitation)

#' 
#' The degree distribution is row and/or column marginals tabulated. If the network is directed the row/column marginals can be different, if the network is undirected then the row and column margins will be the same by definition. Let's make this more concrete.
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
child1_ego<-ego.extract(imitation,1)$A
col<-rep("red",nrow(child1_ego))
col[rownames(child1_ego)!="A"]<-"blue"
gplot(child1_ego,vertex.col=col,main="Child A Focal Actor")
legend("bottomleft",legend=c("Child A","Alters"),col=c("red","blue"),bty="n",pch=19)

#' 
#' Child A has indegree of 2 and outdegree of 3. If we were to symmetrize the network (i.e., make it undirected),
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
gplot(symmetrize(child1_ego),vertex.col=col,main="Child A Focal Actor",gmode="graph")
legend("bottomleft",legend=c("Child A","Alters"),col=c("red","blue"),bty="n",pch=19)

#' Then Child A would have a degree of 3. If we do this for all the children and tabulate the results we will have the degree distribution (for directed we would have the indegree and outdegree distribution). 
#' 
#' ## Directed Case: indegree, outdegree
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
id<-sna::degree(imitation,cmode="indegree")
od<-sna::degree(imitation,cmode="indegree")
deg_mat<-data.frame(child=imitation%v%"vertex.names",indegree=id,outdegree=od)
kable(deg_mat)

#' 
#' This tabulation is pretty neat! What can we learn from it. Indegree represents those children who are being mimicked. So high indegree means other kids are copying you! Child O\* is quite popular, note that he/she/they also mimick others at a high level (highest outdegree)!
#' 
#' If we tabulate the indegree and outdegree, then we have the degree distribution, i.e.,
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
print("Outdegree distribution")
kable(data.frame(table(od)))
hist(od,main="Outdegree Distribution",xlab="Degree")

print("Indegree distribution")
kable(data.frame(table(id)))
hist(id,main="Indegree Distribution",xlab="Degree")

#' 
#' ## Undirected Case: degree
#' 
#' If we symmetrize the data (i.e., make it undirected), then we just have the degree distribution (and indegree and outdegree are now the same).
#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
d<-sna::degree(symmetrize(imitation),cmode="indegree")
deg_mat2<-data.frame(child=imitation%v%"vertex.names",degree=d)
kable(deg_mat2)

#' 
## ----warning=FALSE,message=FALSE,fig.align='center',fig.height=8/1.5,fig.width=12/1.5----
print("Outdegree distribution")
kable(data.frame(table(d)))
hist(d,main="Degree Distribution",xlab="Degree")

#' 
#' 
#' # Mean Degree 
#' 
#' Mean degree is simply the classic average of the degrees in the network.
#' 
#' ## Directed Case, Mean Indegree/Outdegree
#' 
#' So for directed network the Mean Indegree is
#' 
#' $$\bar{d}_i = \frac{1}{n} \sum_{i=1}^n degree_i(v_i)$$
#' and the mean outdegree is
#' 
#' $$\bar{d}_o = \frac{1}{n} \sum_{i=1}^n degree_o(v_i)$$
#' So if we go back to our pre-school example,
#' 
## ------------------------------------------------------------------------
kable(deg_mat)

#' 
#' The mean outdegree is:
#' 
## ------------------------------------------------------------------------
mean(deg_mat$outdegree)

#' 
#' and the mean indegree is:
#' 
## ------------------------------------------------------------------------
mean(deg_mat$indegree)

#' 
#' ## Undirected Case, Mean Degree
#' 
#' Similar to the directed case, mean degree is:
#' 
#' $$\bar{d} = \frac{1}{n} \sum_{i=1}^n degree(v_i)$$
#' and we can calculate it as follwos,
#' 
## ------------------------------------------------------------------------
mean(deg_mat2$degree)

#' 
#' 
#' # Geodesics
#' 
#' ## Preliminaries
#' 
#' The discussion in the following sections will use the `sna` package for all computation, and `networkdata` package for emperical examples.
#' 
#' 
#' ## Geodesics
#' 
#' 
#' > In the mathematical field of graph theory, the distance between two vertices in a graph is the number of edges in a <u>shortest path</u> (also called a graph *geodesic*) connecting them. This is also known as the geodesic distance. Notice that there may be more than one shortest path between two vertices. If there is no path connecting the two vertices, i.e., if they belong to different connected components, then conventionally the distance is defined as infinite.
#' 
#' > In the case of a directed graph the distance $d(u,v)$ between two vertices $u$ and $v$ is defined as the length of a shortest path from $u$ to $v$ consisting of arcs, provided at least one such path exists. Notice that, in contrast with the case of undirected graphs,$d(u,v)$ does not necessarily coincide with $d(v,u)$, and it might be the case that one is defined while the other is not. - [Wikipedia](https://en.wikipedia.org/wiki/Distance_(graph_theory))
#' 
#' 
#' ### Example of Geodesic calculation in R
#' 
## ------------------------------------------------------------------------
args(sna::geodist)

#Find geodesics on a random graph
gd<-geodist(rgraph(5))

#Examine the number of geodesics
gd$counts

#Examine the geodesic distances
gd$gdist


#' 
#' ## Diameter of a Graph
#' 
#' The diameter of a network is the largest distance between any two nodes in the network:
#' 
#' $$diameter = \max_{i,j} d(i, j)$$
#' 
#' ### Example: diameter of a graph in R
#' 
#' 
## ------------------------------------------------------------------------
## Simulate directed and undirected network
digraph<-rgraph(15,tprob=.3)
graph<-rgraph(15,mode="graph",tprob=.6)

## Compute geodesics
geodis.di<-geodist(digraph)
geodis.g<-geodist(graph)

## Compute Diameter
diam.di<-max(geodis.di$gdist)
diam.di
diam.g<-max(geodis.g$gdist)
diam.g

#' 
#' ## Average Path Length
#' 
#' We can define an average path length or distance for a connected graph (component) or a strongly connected (component of a)
#' digraph. Note that we can define it for unconnected graphs by defining the distance between any unconnected set of nodes as zero.
#' 
#' ### Directed
#' 
#' $$ averPathLength(G) = \frac{1}{2{N \choose 2}} \sum_{i,j;i\neq j} d(i,j)$$
#' Where ${N \choose 2} = \frac{N(N-1)}{2}$, the total number of node pairs for a graph of size $N$.
#' 
## ------------------------------------------------------------------------
diag(geodis.di$gdist)<-0
n.d<-NROW(digraph)
total<-2*choose(n.d,2)
avpath.d<-(1/total)*sum(geodis.di$gdist)
avpath.d

#' 
#' ### Undirected
#' 
#' $$ averPathLength(G) = \frac{1}{{N \choose 2}} \sum_{i,j;i\neq j} d(i,j)$$
#' 
#' Where ${N \choose 2} = \frac{N(N-1)}{2}$, the total number of node pairs for a graph of size $N$.
#' 
## ------------------------------------------------------------------------
diag(geodis.g$gdist)<-0
n.g<-NROW(graph)
total<-choose(n.g,2)
avpath.g<-(1/total)*sum(geodis.g$gdist)
avpath.g

#' 
#' ## Emperical Example
#' 
## ------------------------------------------------------------------------
## Function for average path length
averagePathLength<-function(net){
  if(!is.network(net)){stop("Not a Network")}
  gd<-geodist(net)
  if(net%n%"directed"){
    return((1/choose(network.size(net),2))*sum(gd$gdist))
    }
  (1/(2*choose(network.size(net),2)))*sum(gd$gdist)
}

## Function for diameter
diameter<-function(net){
  gd<-geodist(net)
  max(gd$gdist)
}


#' 
## ------------------------------------------------------------------------
data(beach)
beach[[1]]%n%"directed"
## Ooops! This is an undirected network!
beach[[1]]%n%"directed"<-FALSE

## Compute Average Path Length
averagePathLength(beach[[1]])

## Compute Diameter
diameter(beach[[1]])

