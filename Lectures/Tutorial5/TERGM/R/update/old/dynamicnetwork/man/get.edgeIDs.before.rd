\name{get.edgeIDs.before}
\alias{get.edgeIDs.before}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get the id of the most recent edge active before a time }
\description{
  gets the id of the edge in the passed network (or vertex neighborhood) that 
  was most recently active. (name is confusing)
}
\usage{
get.edgeIDs.before(x, edge.time, v, alter = NULL, neighborhood = c("out", "in", "combined"), na.omit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ dynamicnetwork to search for edges }
  \item{edge.time}{ the time point of interest }
  \item{v}{ optional vertex id for neighborhood }
  \item{alter}{ optional second vertex }
  \item{neighborhood}{ type of ngh to search (if netowrk is directed) }
  \item{na.omit}{ should na edges be skipped? }
}
\details{
  This function is an adaptation of the get.edgeIDs function for dynamic networks.  
  For \code{get.edgeIDs.at}, it returns the id of the most recently active edge
  that meets the criteria,  having a valid intervals with start < time.point. 
  If two edges are tied, one is picked arbitrarily. If an edge as an open interval
  (end time = NA) it will be chosen.   Used to determine appropriate edge to 
  modify during network construction.   
}
\value{
  the id of the the most recently active edge before the time specified, or 
  \code{numeric(0)} if none meet the criteria.
}

\author{ skyebend@skyeome.net }

\seealso{  \code{\link{get.edgeIDs.at}} }
\examples{

#make a silly network
dyn <- as.dynamic(network.initialize(5));
dyn <- add.edge.dynamic(dyn,0,1,2);
dyn <- add.edge.dynamic(dyn,1,1,3);
dyn <- add.edge.dynamic(dyn,2,1,4);
dyn <- add.edge.dynamic(dyn,3,1,5);
# see all edge edges that include vertex 1 
get.edgeIDs(dyn,v=1)   #should be 4 3 2 1
#plot the intervals
plot.intervals(dyn);  #notice edgeid 1 goes from -1 to 0
#get the most recent edge active before time 2
get.edgeIDs.before(dyn,v=1,edge.time=2);# should be id 3
  }

\keyword{ graphs }
