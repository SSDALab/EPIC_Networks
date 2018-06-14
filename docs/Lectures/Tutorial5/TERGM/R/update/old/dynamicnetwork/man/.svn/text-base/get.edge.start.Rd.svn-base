\name{get.edge.start}
\alias{get.edge.start}
\alias{get.edge.end}
\alias{set.edge.start}
\alias{set.edge.end}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ read or modify the times assigned to an exisiting edge}
\description{
  convinience methods for reading or modifying edge times without directly accessing the list, makes it possible to change implementation in the future. 
}
\usage{
get.edge.start(network, edgeID)
get.edge.end(network, edgeID)
set.edge.start(network,edgeID,valid.time)
set.edge.end(network,edgeID,valid.time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{  \code{network} the network the edge belongs to }
  \item{edgeID}{  \code{edgeID} the ID of the edge }
  \item{valid.time}{  \code{edgeID} (for setters) the start (or end) value for the edge }
}
\details{
   These methods make it possible to access and modify the valid intervals 
   associated with edges.  It is much preferable to use these methods instead of 
   directly modifing the objects. 
  For dynamic network objects the edge timing information is stored on a list
  named 'et' (edge time list) that parallels the master edge list.  The timing
  information for an edge will be at the same index on tel as the edge index on mel.
   
}
\value{
  get methods will return a single numeric value indicating either the starting 
  time or ending time of the edge.  'NA' can also be used to indicate that it is
  unknown or undefined.  
  
  set methods will return the network object containing the newly modified edge
}

\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}  }
\note{ 
the ETL list is not backended in C, so it is important to be 
 careful with the network objects returned 
}
\examples{

  #make a network
   myNet <- network.initialize(5);
  #add some edges
    myNet[1,2] <-1;
    myNet[2,3] <-1;
    myNet[2,4] <-1;
    myNet[3,5] <-1;
    #convert it to dynamic, will have the bounds [0,1]
    myDyn <- as.dynamic(myNet);
    #check the end of the first edge
    get.edge.end(myDyn,1);
    #check the start of the 2nd edge
    get.edge.start(myDyn,2);
    #change the first edge
    myDyn <- set.edge.start(myDyn, 1, 2); #whoops, we made an invalid interval [2,1] !
    myDyn <- set.edge.end(myDyn,1,5);
    #print the etl to see if it worked
    print(myDyn$etl);
    #plot the valid intervals
    plot.intervals(myDyn);
   
}
\keyword{ graphs }% at least one, from doc/KEYWORDS
