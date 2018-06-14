\name{get.edges.difference}
\alias{get.edges.difference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get ids of edges occuring in both networks }
\description{
  Given two networks, returns a vector of edgeIDs for edges in the second network that do not have a corresponding edge in the first.  Mostly used for comparing the same network at two points in time in order to get a list of newly created edges.
}
\usage{
get.edges.difference(network1, network2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network1}{ first network to compare }
  \item{network2}{ second network to compare, ids will be from this network }
}
\details{
  Uses the \code{\link{get.matching.edgeIDs}} function to find edges in both 
  networks that have the same head and tail sets.  Returns the ids of all edges
  in the second network that do not have a match in the first. 
}
\value{
  vector of ids in the second network that do not have a match in the first. 
}
\references{ ~put references to the literature/web site here ~ }
\author{  Skye Bender-deMoll \email{skyebend@skyeome.net} }

\seealso{  \code{\link{get.edges.intersection}} }
\examples{
   library(sna);
#make two random graphs
  graph1 <- as.network(rgraph(10));
  graph2 <- as.network(rgraph(10));
  #find the edges in graph2 that are not in graph 1
  diff <- get.edges.difference(graph1,graph2)}

      }
\keyword{ graphs }

