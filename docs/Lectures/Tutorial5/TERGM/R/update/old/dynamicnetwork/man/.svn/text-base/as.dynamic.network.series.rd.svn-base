\name{as.dynamic.network.series}
\alias{as.dynamic.network.series}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Convert a network.series or list of networks into dynamic network }
\description{
  \code{as.dynamic.network.series} adds additional data stuctures to network 
  objects to represent changes in edge sets and attributes over time. 
}
\usage{
as.dynamic.network.series(net,check.renewal=TRUE,... )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ an object of class \code{network.series}}
  \item{check.renewal}{ should the construction algorithm check if the same edge will be toggled multiple times }
  \item{\dots} {additional arguments for generics}
}
\details{
Actually calls method \code{as.dynamic.list} with the networks argument of the 
 \code{network.series}. The first network of the series is copied and then edges 
 from all subsequent networks in the series are copied in with appropriate time 
 values.  Note: for now both kinds of networks are assumed to be non-renewal.
}
  
\value{
   a \code{\link{dynamicnetwork}} object
}

\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}}

\seealso{ \code{\link{dynamicnetwork}} \code{as.dynamic.list}}
\examples{
#NOTE: statnet must be loaded before this package so that it will not mask
\dontrun{
%#library(statnet);
%#Load florentine data
%data(florentine);
%#create an ergm model for the data
%g.est <- ergm(flomarriage ~ edges + kstar(2));
%#simulate a few steps of model evolution
%g.sim <- simulate(g.est,nsim=10,burnin=1000,interval=10);
%#convert the network.series into dynamic.network
%g.dyn <- as.dynamic(g.sim);    #hmm, I think this would actually call a differnt method?
}
}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
