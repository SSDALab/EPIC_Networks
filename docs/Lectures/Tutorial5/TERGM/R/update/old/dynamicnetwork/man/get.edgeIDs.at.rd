\name{get.edgeIDs.at}
\alias{get.edgeIDs.at}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get edges active at a specific time }
\description{
  gets the ids of edges in the passed network (or vertex neighborhood) that are active 
  at the appropriate specified time.  
}
\usage{
get.edgeIDs.at(dyn, time.point, v, alter = NULL, neighborhood = c("out", "in", "combined"), na.omit = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dyn}{ dynamicnetwork to search for edges }
  \item{time.point}{ the time point of interest }
  \item{v}{ optional vertex id for neighborhood }
  \item{alter}{ optional second vertex }
  \item{neighborhood}{ type of ngh to search (if netowrk is directed) }
  \item{na.omit}{ should na edges be skipped? }
}
\details{
  This function is an adaptation of the get.edgeIDs function for dynamic networks.  
  For \code{get.edgeIDs.at}, it returnes ids of edges that have 
  have intervals with start < time.point and end >= time.point.  For \code{get.edgeIDs.before}
}
\value{
  a list of edge ids active at the specified time and in the appropriate neighborhood
}

\author{ skyebend@skyeome.net }

\seealso{  \code{\link{get.edgeIDs.before}},\code{\link{get.slice.network}} }
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
#get the edge active at time 1
get.edgeIDs.at(dyn,v=1,time.point=1);# should be id 2
}

\keyword{ graphs }
