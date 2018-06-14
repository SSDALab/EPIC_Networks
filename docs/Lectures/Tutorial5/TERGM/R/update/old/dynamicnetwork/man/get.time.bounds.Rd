\name{get.time.bounds}
\alias{get.time.bounds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ find the bounding time interval for a dynamic network}
\description{
  Finds the smallest and largest time values in any of the edges and attribute 
  in the network.  Checks first if argument is dynamic. 
}
\usage{
get.time.bounds(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a dynamic network to be checked }
}
\details{
  OOPS, still does't check timing on dynamic edge attributes!
}
\value{
  A two elment vector [start,end] with the smallest and largest time values of the network
}
\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}  }

\examples{
  ##data(faux
}
\keyword{ graphs }
