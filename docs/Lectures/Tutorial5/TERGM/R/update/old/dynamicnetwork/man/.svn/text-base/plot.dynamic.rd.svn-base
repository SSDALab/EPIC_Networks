\name{plot.dynamic}
\alias{plot.dynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plot snapshots of a dynamic network }
\description{
  plots specified time period of a dynamic network 
}
\usage{
plot.dynamic(x, time.point = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the dynamicnetwork to plot }
  \item{time.point}{ the point in time to view the network }
  \item{\dots}{ additional arguments to plot.network }
}
\details{
  Uses \code{\link{get.slice.network}} and \code{plot.network} to plot the
  specified time slice of the network.  If \code{time.point} is ommited, plots 
  the entire graph as a multiplex network. 
}
\value{
  Draws plot of network in plot window, silently returns coords of plot. 
}
\references{ ~put references to the literature/web site here ~ }
\author{ skyebend@skyeome.net }

\seealso{ \code{plot.network} in network package for details of network plotting,
\code{\link{get.slice.network} for an explanation of treatment of dynamic attributes,
\code{\link{plot}}, and \code{\link{par}} for general plotting params }
}
\examples{

#we want to create a dynamic network with 10 nodes and no edges
dyn <- as.dynamic(network.initialize(10));
#now we add an edge with a time interval 1 to 10, linking nodes 1 and 2
dyn<-add.edge.dynamic(dyn,c(1,10),1,2);
#now we add a bunch more edges with various times
dyn<-add.edge.dynamic(dyn,c(2,10),2,3);

dyn<-add.edge.dynamic(dyn,c(3,10),3,4);
dyn<-add.edge.dynamic(dyn,c(4,10),4,5);
dyn<-add.edge.dynamic(dyn,c(5,10),5,6);
dyn<-add.edge.dynamic(dyn,c(6,10),6,7);
dyn<-add.edge.dynamic(dyn,c(6,10),7,8);
dyn<-add.edge.dynamic(dyn,c(7,10),8,9);
dyn<-add.edge.dynamic(dyn,c(8,10),9,1);
dyn<-add.edge.dynamic(dyn,c(9,10),10,1);
dyn<-add.edge.dynamic(dyn,c(9,20),10,1);
dyn<-add.edge.dynamic(dyn,c(9,20),10,5);
dyn<-add.edge.dynamic(dyn,c(9,20),10,3);
dyn<-add.edge.dynamic(dyn,c(9,20),10,8);
#now add some attributes
#first give vertex ten the color blue at time 8
dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time=8,v=10);
dyn <- set.dynamic.vertex.attribute(dyn, "color","green",valid.time=9,v=10);
dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time=10,v=10);

#now plot a series of slices through our example network to show it at various times
par(mfrow=c(3,3)); # set up to show 9 plots
for(p in 5:13){
  slice <-get.slice.network(dyn,p);
  plot(slice,vertex.col=get.vertex.attribute(slice,"color"),vertex.cex=5, main=p);
 }
}
\keyword{ graphs }

