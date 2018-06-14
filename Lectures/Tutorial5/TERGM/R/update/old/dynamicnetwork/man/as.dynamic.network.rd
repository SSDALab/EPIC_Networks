\name{as.dynamic.network}
\alias{as.dynamic.network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert networks or sets of networks into dynamic network }
\description{
  \code{as.dynamic.network} adds additional data stuctures to network objects to represent changes in edge sets and attributes over time. 
}
\usage{
as.dynamic.network(net,edge.times = c(0,1),check.renewal = TRUE,... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ an object of class \code{network} to be converted}
  \item{edge.times}{ time values to be assigned to elements }
  \item{check.renewal}{sets if code should check for renewal ties}
  \item{...}{additional arguments for generic compatibility}
}
\details{
For \code{network} object, a copy is created, the appropriate class and datastructures are added, and the times of all edges are set to the value of the parameter edge.times.  Note: for now both kinds of networks are assumed to be non-renewal so some information may be lost if a series representing a renewal network is passed in. \code{network}.
}
\value{
  \code{\link{dynamicnetwork}} object; should not modify its argument networks
}
\references{  }
\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}}
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~
                                                   
\seealso{ \code{\link{dynamicnetwork}},\code{network}}
\examples{

#make a network
net <- network.initialize(5);
#add some edges
net[1,0] <- 1;
net[1,2] <- 1; 
net[2,3] <- 1;
#make it dynamic with edges lasting from 0 to 6
g.dyn <- as.dynamic.network(net, edge.times=c(0,2));

}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
