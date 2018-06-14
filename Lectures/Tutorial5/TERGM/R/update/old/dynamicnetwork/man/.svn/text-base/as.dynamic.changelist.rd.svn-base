\name{as.dynamic.changelist}
\alias{as.dynamic.changelist}
\title{ converts a matrix of edge changes into a dynamic network }
\description{
  Creates a dynamicnetwork from a starting network and matrix of edge changes
  that describe the evolution in time. 
}
\usage{
as.dynamic.changelist(changelist,startnet=NULL,nNodes=NULL, renewal = TRUE, last.time = max(changelist[, 1]) + 1, subsample = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{changelist}{ a three column matrix [time step, head, tail] indicating toggles }
  \item{startnet}{ the (optional)base network that the changes will start from }
  \item{nNodes}{if no start net is specified, how many nodes should the network have?}
  \item{renewal}{ is the network renewal? }
  \item{last.time}{ ending time of the network (default is last change +1) }
  \item{subsample}{ optional vector restricted set of ids to use }
  \item{\dots}{ optional future arguments}
}
\details{
  Converts a change list matrix (set of toggles) into a dynamic network. The input is a three column matrix (time step, head node, tail node)  There is one row for each changed dyad over the entire series (i.e., time to the last time). Values listed under time step i are the changes made to the network at time step i to make it the network at time i+1. They are listed in time step order.  This is the method used to convert the output of the in house ERGM simulation.
  
  If a startnet is included attributes will be copied to the dynamic network, 
  otherwise it is assumed to be starting from an empty network, the size of 
  this network will be taken from the nNodes argument. 

 When the  'subsample' argument is set to a vector of node ids, only relations involving those ids will be inserted into the the network, making it possible to easily extract a subsample of relations from a much larger network. 
 
}
\value{
  a \code{\link{dynamicnetwork}} object. 
}
\references{ ~put references to the literature/web site here ~ }
\author{ skyebend@skyeome.net}

\seealso{  \code{as.dynamic.network},\code{as.dynamic.intervals},
\code{as.dynamic.intervals} }
\examples{
changelist <- matrix(ncol=3,nrow=6)
changelist[1,] <-c(0,1,2);
changelist[1,] <-c(0,2,3);
changelist[1,] <-c(1,3,4);
changelist[1,] <-c(2,3,1);
changelist[1,] <-c(0,1,2);
changelist[2,] <-c(0,2,3);
changelist[3,] <-c(1,3,4);
changelist[4,] <-c(2,3,1);
changelist[5,] <-c(3,1,2);
changelist[6,] <-c(3,2,1);

#build the dynamic network, specifying a 4 node net
dyn <- as.dynamic.changelist(changelist,nNodes=4);

#make the same network, passing in an empty network
dyn <- as.dynamic.changelist(changelist,startnet=network.initialize(4));
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ graphs }
