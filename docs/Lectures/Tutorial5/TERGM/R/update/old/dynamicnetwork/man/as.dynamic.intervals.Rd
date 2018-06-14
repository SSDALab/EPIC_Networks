\name{as.dynamic.intervals}
\alias{as.dynamic.intervals}
\title{ Convert sets of valid intervals into dynamic network}
\description{
  Converts a set of valid intervals in the form of four-column matrix
with columns source, target, start, end, into a dynamic network object.
}
\usage{
as.dynamic.intervals(interval.list, netsize, subsample = NULL, directed = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{interval.list}{ matrix with 4 columns giving the source, target, start, end for each relation }
  \item{netsize}{ the number of verticies in the network }
  \item{subsample}{ a vector giving a subsample of verticies to include }
  \item{directed}{ should the constructed network be directed? }
  \item{\dots}{ additional arguments for constructing a network object }
}
\details{
  The interval list provides a list of relations in a dynamic network.  
  The first two columns give the ids of the source and target verticies of the 
  edge, and the the remaining two the starting and ending time for the relation.
}
\value{
  a \code{\link{dynamicnetwork}} object
}
\references{ ~put references to the literature/web site here ~ }
\author{ Skye Bender-deMoll, the CSDE statnet team }

\seealso{  \code{\link{as.dynamic.changelist}} }
\examples{
#build up an intervals matrix for 6 relations
intervals <- matrix(nrow=6,ncol=4);
#a relation from node 1 to node 2 from time 0 to time 10
intervals[1,] <- c(1,2,0,10);
intervals[2,] <- c(2,3,5,9);
intervals[3,] <- c(3,4,4,10);
intervals[4,] <- c(1,4,8,10);
intervals[5,] <- c(4,1,5,8);
intervals[6,] <- c(2,4,9,10);
#convert to dynamic network with 4 nodes
dyn <- as.dynamic.intervals(intervals,netsize=4);
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }

