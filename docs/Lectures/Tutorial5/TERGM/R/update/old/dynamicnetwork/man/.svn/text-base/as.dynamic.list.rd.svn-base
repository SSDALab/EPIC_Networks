\name{as.dynamic.list}
\alias{as.dynamic.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ convert a list of static networks into a dynamic network }
\description{
  Takes a list of network objects describing the descrete time evolution of a 
  a network and creates an appropriate dynamic network object.  
}
\usage{
as.dynamic.list(net, check.renewal = TRUE, renewal = FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ a list of network objects (confusing name for generic compatibility) }
  \item{check.renewal}{ should the list be checked to see if it is describing a renewal network? }
  \item{renewal}{should the created dynamc network be renewal?}
  \item{\dots}{ for compatibility with generics }
}
\details{
  The dynamc network will have network attributes copied from the first network 
  on the list. Assumes the first network is describing the time interval [0,1]. 
  Assumes that edges that are `on' in successive networks describe observations
  of an edge that remains on, and extends the interval accordingly. 
  If \code{check.renewal} is set, will give errors if input list describes a 
  network with renewing relations.  All networks in the list must have the same 
  size
}
\value{
  a \code{\link{dynamicnetwork}} object describing the evolution of the edgeset
}
\references{ ~put references to the literature/web site here ~ }
\author{ skyebend@skyeome.net }
\note{ does not yet convert attributes that change on the network's edges or 
verticies  into dynamic attributes
}
\seealso{ \code{\link{as.dynamic.changelist}}, \code{\link{as.dynamic.intervals}} }
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }

