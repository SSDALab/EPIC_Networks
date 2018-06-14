\name{set.dynamic.vertex.attribute}
\alias{set.dynamic.vertex.attribute}
\title{ set an attribute in a dynamic network at a specific time}
\description{
  Specifies that the named attribute in the dynamic network should take on the given value at the givin time and for all later times until the next value
}
\usage{
set.dynamic.vertex.attribute(net, attrname, value, valid.time = NULL, v = 1:get.network.attribute(net, "n"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ \code{net} \code{\link{dynamicnetwork}} that the attribute should be set in}
  \item{attrname}{  \code{attrname} String giving the name of the attribute }
  \item{value}{  \code{value} Object to be used as value }
  \item{valid.time}{  \code{valid.time} integer when value should take effect. If null calls \code{set.vertex.attribute} from the network package instead  with the same parameters }
  \item{v}{  \code{v} indicies of verticies to which the attribute should be added }
  }
\details{
  This is implemented by adding attributes to 'val' that are [value, time] x num changes matricies. So adding additional values adds more rows to the matrix.  (deletion behavior not defined, but should be ok)  These attributes are considered 'dynamic attributes', defined by having the vertex name listed in the graph-level attribute 'dynam.attr.names'.  Uses the interal set.vertex.attribute method to set the value. The attribute is assumed to have the same value from valid.time until the next specified time for the named attribute on a vertex.    
}
\value{
 returns the network with the value set
}
\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}, CSDE statnet team }
\note{ This is a very naive implementation that assumes the variables will be added in increasing time order.  Violating this should give a warning and then strange results.  Easy to do this better in C.

  \section{Warning }{This is very alpha, slow, and should be implemented in the backend using C} 
}
\seealso{  \code{set.vertex.attribute} in the network package,\code{\link{get.slice.network}} }
\examples{
 
   dyn <- as.dynamic(network.initialize(2)); # a two node network
   #give vertex one the color blue at time one. 
   dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time = 0,v=1);
   dyn <- set.dynamic.vertex.attribute(dyn, "color","red",valid.time = 1,v=1);
   dyn <- set.dynamic.vertex.attribute(dyn, "color","green",valid.time = 2,v=1);
}
\keyword{ graphs }% at least one, from doc/KEYWORDS
