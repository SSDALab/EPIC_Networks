\name{print.dynamic}
\alias{print.dynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ prints a dynamicnetwork object to console in summary form }
\description{
  bascially calls print.network, but adds the time range, etc. 
}
\usage{
print.dynamic(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ dynamicnetwork object to be printed }
  \item{\dots}{ additional arguments }
}

\value{
  prints output to console, returns text silently
}
\author{ skyebend@skyeome.net }

\seealso{  \code{print.network} in the network package }
\examples{
print(as.dynamic(network.initialize(5))); #will produce warning as there are no edges on network for time range
}
\keyword{ graphs }

