\name{is.dynamic}
\alias{is.dynamic}

\title{ check if a network object is a dynamic network }
\description{
  check if an object has the appropriate class to be considered a dynamicnetwork
}
\usage{
is.dynamic(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ an object (presumably a network) }
}
\details{
  only checks class attribute, does not verify data structure
}
\value{
  returns TRUE if object inherits the class \code{dynamic}.
}
\author{ skyebend@skyeome.net }

\examples{

is.dynamic(list()); #FALSE
is.dynamic(network.initialize(5)); #FALSE
is.dynamic(as.dynamic(network.initialize(5))); #TRUE

}

\keyword{ graphs }
