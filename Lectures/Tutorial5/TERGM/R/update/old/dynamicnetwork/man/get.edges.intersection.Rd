\name{get.edges.intersection}
\alias{get.edges.intersection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ finds edges in second network that are also in the first }
\description{
  Given two networks, returns a vector of edgeIDS for edges in the second network that have a corresponding edge in the first network.  Mostly used for comparing the same network at two points in time to find the edges that have not changed. 
}
\usage{
get.edges.intersection(network1, network2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network1}{ first network to compare }
  \item{network2}{ second network to compare, edge ids returned will be fore this network }
}
\details{
  Uses the \code{\link{get.matching.edgeIDs}} function to find edges in both 
  networks that have the same head and tail sets.  Returns the ids of all edges
  in the second network  have a match in the first. 
}
\value{
  Vector of edgeids in the second network that have matches in the first. 
}
\references{ ~put references to the literature/web site here ~ }
\author{  Skye Bender-deMoll \email{skyebend@skyeome.net} }

\seealso{  \code{\link{get.edges.difference}}}
\examples{
 library(sna);
#make two random graphs
  graph1 <- as.network(rgraph(10));
  graph2 <- as.network(rgraph(10));
  #find the edges in graph2 that are not in graph 1
  diff <- get.edges.intersection(graph1,graph2)}
}
\keyword{ graphs }% at least one, from doc/KEYWORDS

