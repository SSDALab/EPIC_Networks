\name{get.matching.edgeIDs}
\alias{get.matching.edgeIDs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ returns edges in second network that match the id in the first }
\description{
     Method for searching a network for edge(s) that match up with an edge in 
     another network.  
}
\usage{
get.matching.edgeIDs(edgeID, network1, network2)
}

\arguments{
  \item{edgeID}{ edge id in first network that we are checking for a match }
  \item{network1}{ first network }
  \item{network2}{network we are searching in for a match }
}
\details{
  When searching for matches, we can't use any kind of strict equality measure because
     we are talking about differnt network objects.  For these purposes, two edges match 
     if they have the same head and tails sets, according to \code{\link{setequal}}.
     (This assumes that the ids of nodes in both networks match to the same entities)
     Because networks may be multiplex, it can return more than one match.  Should be 
     safe for hyper edges because it is doing the set comparison.  
}
\value{
  A vector of ids of matching edges in the second network, or NULL if no matches
  are found. 
}
}
\author{  Skye Bender-deMoll \email{skyebend@skyeome.net} }
\note{ ~~further notes~~ 
 It would be great to have a more comprehensive discussion of differnt kinds
 of node and edge equality in dynamic setting. 
}
\examples{
   library(sna);
   #make a network  (hopefully it won't be an empty graph!)
   net <- as.network(rgraph(10));
   #make another
   net2 <- as.network(rgraph(10));
   #is there an edge in net 2 that matches with edge #1 in net?
   eid <- get.matching.edgeIDs(1,net,net2);
}
\keyword{ graphs }
