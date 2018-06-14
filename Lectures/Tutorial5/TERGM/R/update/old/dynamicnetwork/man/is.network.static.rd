\name{is.network.static}
\alias{is.network.static}
\title{ checks that a network is not dynamic}
\description{
  checks that its argument is a network, but is not a dynamic network.
}
\usage{
is.network.static(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ the object (presumeably a network) to check }
}
\details{
  Only checks that argument is a network and is not dynamic, does not check
  actual datastructure for integrity
}
\value{
  TRUE if argument is a \code{network} and is not a \code{\link{dynamicnetwork}}
}

\author{ skyebend@skyeome.net }

\seealso{ \code{is.network} in the network package, \code{\link{is.dynamic}} }
\examples{

is.network.static(list());  #FALSE
is.network.static(network.initialize(5)); #TRUE
is.network(as.dynamic(network.initialize(5))); #TRUE, is a network, AND dynamic
is.network.static(as.dynamic(network.initialize(5))); #FALSE
}

\keyword{ graphs }

