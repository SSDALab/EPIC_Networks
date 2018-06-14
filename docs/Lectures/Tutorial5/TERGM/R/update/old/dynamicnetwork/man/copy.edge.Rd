\name{copy.edge}
\alias{copy.edge}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ copy sets of edges between networks }
\description{
  Creates edges in the target network with vertex sets and attributes corresponding to the edges with given ids in the source network. If the first network is not dynamic, the second network is, and no time is specified, the times will be copied as well.  
}
\usage{
copy.edge(network1, edgeIDs, network2, time2 = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network1}{  \code{network1} source network }
  \item{edgeIDs}{  \code{edgeIDs} ids of of edges in the source network }
  \item{network2}{ \code{network2} the target network }
  \item{time2}{  \code{time2} if the the target network is dynamic, the time to be assigned to the edges  }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  a the target network with the new edge added
}
\references{ ~put references to the literature/web site here ~ }
\author{ skyebend@skyeome.net }

\seealso{  \code{\link{add.edge.dynamic}} }
\examples{

}
\keyword{ graphs }% at least one, from doc/KEYWORDS
