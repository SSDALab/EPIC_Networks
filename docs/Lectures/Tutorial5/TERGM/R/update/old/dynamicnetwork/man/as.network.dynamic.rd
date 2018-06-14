\name{as.network.dynamic}
\alias{as.network.dynamic}
\alias{as.static.network}
\title{ coearce dynamicnetwork into static network }
\description{
  Forces a dynamic network to appear as a static network by changing its class
  without altering data.  
}
\usage{
as.network.dynamic(x, ...)
as.static.network(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ dynamic network to be converted }
  \item{\dots}{ possible other arguments }
}
\details{
  Class of passed network is modified to convert it to a network without actually
  doing any conversion or aggregation of the nettwork attributes.  Will appear
  as a `regular' network with multiplex edges.  
}
\value{
  a a static network object
}
\references{ ~put references to the literature/web site here ~ }
\author{ skyebend@skyeome.net }

\seealso{  \code{\link{get.slice.network}} to get a network snapshot of a dynamicnetwork at a 
specific time with appropriate attributes }
\examples{
  dyn <- as.dynamic(network.initialize(5));   #make a dynamic network
  net <- as.network(dyn);  #convert it back
}
\keyword{ graphs}

