\name{set.dynamic.edge.attribute}
\alias{set.dynamic.edge.attribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ add dynamic attributes to edges}
\description{
  Specifies that the named attribute on the edge in the dynamic network should 
  take on the given value at the givin time and for all later times until the next value
}
\usage{
set.dynamic.edge.attribute(net, attrname, value, valid.time = NULL, e = 1:length(net$mel))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ dynamicnetwork the attribute will be attached to }
  \item{attrname}{ name for the attribute }
  \item{value}{ value of the attribute }
  \item{valid.time}{ the time at which the attribute should take on this value }
  \item{e}{ ids of edges that value should be attached to }
}
\details{
    This is implemented by adding attributes to edges (on the `\code{mel}' list that
     are [value, time] x num changes matricies. So adding additional values adds more 
     rows to the matrix.  (deletion behavior not defined, but should be ok)  
     These attributes are considered `dynamic attributes', defined by having the 
     vertex name listed in the graph-level attribute \code{dyn.edge.attr.names}.  
     Uses the interal set.edge.attribute method to set the value. The attribute is 
     assumed to have the same value from valid.time until the next specified time
      for the named attribute on an edge.  
}
\value{
  The dynamic network, modified to include the dynamic attribute value
}

\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}, CSDE statnet team }

\seealso{  \code{\link{set.dynamic.vertex.attribute}},\code{\link{get.slice.network}} }
\examples{
#create a dynamic network
   dyn <- as.dynamic(network.initialize(5));
#add an edge from 1 5  from time 1 to time 10 
   dyn <-add.edge.dynamic(dyn,c(1,10),1,5);
#add an attributes named "testing" to edge number 1 at time 1
   dyn <- set.dynamic.edge.attribute(dyn,"testing","1",1,1);
   dyn <- set.dynamic.edge.attribute(dyn,"testing","2",4,1);
   dyn <- set.dynamic.edge.attribute(dyn,"testing","3",6,1);
   
#get the value in the sliced network at time 2
   get.slice.network(dyn,2)%e%"testing";    #should be 1
   get.slice.network(dyn,4)%e%"testing";    #should be 2
}
\keyword{ graphs}

