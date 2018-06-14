\name{plot.intervals}
\alias{plot.intervals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plots valid intervals as a timeline }
\description{
  Plots a timeline view of the timing information attached to edges and verticies of a graph
}
\usage{
plot.intervals(x, time.range = NULL, show.vertex = TRUE, pickSlice = FALSE, showAttribute = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ The graph to plot }
  \item{time.range}{ time range of the graph to show on the plot (defaults to entire range) }
  \item{show.vertex}{ should vertex times also be shown on the plot }
  \item{pickSlice}{ if true, clicking on the timeline will plot the graph for that time point }
  \item{showAttribute}{ not currently used }
  \item{\dots}{ additional plotting arguments }
}

\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\author{ skyebend@skyeome.net }

\seealso{  \code{\link{get.slice.network}}, \code{\link{plot.dynamic}} }
\examples{
dyn <- as.dynamic(network.initialize(10)); #make a dynamic network
dyn<-add.edge.dynamic(dyn,c(1,10),1,2); #slowly and painfully add edges
dyn<-add.edge.dynamic(dyn,c(2,10),2,3); # the 2nd argument is a 2 element
dyn<-add.edge.dynamic(dyn,c(3,10),3,4); # vector giving start and end time
dyn<-add.edge.dynamic(dyn,c(4,10),4,5); # for the edge that is added. 
dyn<-add.edge.dynamic(dyn,c(5,10),5,6);
dyn<-add.edge.dynamic(dyn,c(6,10),6,7);
dyn<-add.edge.dynamic(dyn,c(6,10),7,8);
dyn<-add.edge.dynamic(dyn,c(7,10),8,9);
dyn<-add.edge.dynamic(dyn,c(8,10),9,1);
dyn<-add.edge.dynamic(dyn,c(9,10),10,1);
dyn<-add.edge.dynamic(dyn,c(9,20),10,1);
dyn<-add.edge.dynamic(dyn,c(9,20),10,5);
dyn<-add.edge.dynamic(dyn,c(9,20),10,3);
dyn<-add.edge.dynamic(dyn,c(9,20),10,8);
plot.intervals(dyn);
}
\keyword{ graphs }

