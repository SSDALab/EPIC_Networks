\name{get.spells.for}
\alias{get.spells.for}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get the valid intervals for a neighborhood or node pair }
\description{
  returns a matrix giving the starting and ending times of all edges associated
  with the given vertex or vertex pair. 
}
\usage{
get.spells.for(net, v, alter = NULL, neighborhood = c("out", "in", "combined"), na.omit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{ a \code{\link{dynamicnetwork}} object }
  \item{v}{ id number of the vertex of interest }
  \item{alter}{ optional id of additional vertex }
  \item{neighborhood}{ how the neighborhood should be defined if network is directed }
  \item{na.omit}{ should na edges be skipped? }
}

\value{
  a two column matrix (in which the first column is the start and second column
  is the end) of the valid intervals  corresponding to edges in the specified 
  neighborhood.
}
\author{ skyebend@skyeome.net }

\seealso{  \code{\link{get.edges.spellmatrix}} }
\examples{
#make a toy network
dyn <- as.dynamic(network.initialize(5));
dyn <- add.edge.dynamic(dyn,0,1,2);
dyn <- add.edge.dynamic(dyn,1,1,3);
dyn <- add.edge.dynamic(dyn,2,1,4);
dyn <- add.edge.dynamic(dyn,3,1,5);

#get the valid intervals for any edges between 1 and 3
get.spells.for(dyn,v=1,alter=3);

#get the valid intervals for any edges invloving vertex 1
get.spells.for(dyn,v=1);
}

\keyword{ graphs }

