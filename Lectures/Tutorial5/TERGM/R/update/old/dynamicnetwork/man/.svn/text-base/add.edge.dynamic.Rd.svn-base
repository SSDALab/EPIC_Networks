\name{add.edge.dynamic}
\alias{add.edge.dynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ adds an edge to a dynamic network with starting and ending times }
\description{    Adds and edge to the network that is only 'active' during the specified time interval.  
  
}
\usage{
add.edge.dynamic(x, time, tail, head, names.eval = NULL, vals.eval = NULL, edge.check = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ \code{x} a \code{\link{dynamicnetwork}} to which the edge should be added}
  \item{time}{  \code{time} the time (interval [start,end]) describing the edge's existance }
  \item{tail}{  \code{tail} indicies of verticies in the tailset for the edge }
  \item{head}{ \code{head} indicies of verticies in the headset for the edge}
  \item{names.eval}{  \code{names.eval} list of names for  edge attributes }
  \item{vals.eval}{  \code{vals.eval} list of values for edge attributes }
  \item{edge.check}{  \code{edge.check} sanity check? }
}
\details{
  Edge are added using the normal add.edge internal call, but the time values c(start,end) are stored on an Edge Time List  that has entries in the same order as the mel list.  see \code{\link{dynamicnetwork}}
}
\value{
  returns the network with the newly added edge. 
}
\references{ ~put references to the literature/web site here ~ }
\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}, CSDE statnet team }
\note{ CURRENT IMPLEMENTED ONLY FOR NON-RENEWAL NETS 

 \section{Warning }{THE EDGE TIME LIST IS NOT BACKENDED IN C LIKE THE OTHER COMPONENTS! This means that you have to be really carful about assingments, copying, etc. } 
}
\seealso{  see \code{add.edge} in network package, \code{\link{dynamicnetwork}} }
\examples{
dyn <- as.dynamic(network.initialize(10)); #make a dynamic network
dyn<-add.edge.dynamic(dyn,c(1,10),1,2); #slowly and painfully add edges
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
#now add some atttributes
dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time=8,v=10)
dyn <- set.dynamic.vertex.attribute(dyn, "color","green",valid.time=9,v=10)
dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time=10,v=10)
#plot some of the networks
par(mfrow=c(3,3));
for(p in 5:13){
 slice <-get.slice.network(dyn,p);
 plot(slice,vertex.col=get.vertex.attribute(slice,"color"),vertex.cex=5, main=p);
}

}
\keyword{ graphs }% at least one, from doc/KEYWORDS
