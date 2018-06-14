\name{dynamicnetwork}
\alias{dynamicnetwork}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Class for representing longitudinal networks }
\description{
  \code{dynamic network} object for representing networks having changes in edge 
  sets and attributes over time. 
}

\details{
Current based on a \code{network} object but with an additional Edge Time List 
'etl' giving starting and ending times for each edge. The intervals are defined 
as vector \code{[start,end]} where start must be <= end. (tho' this is not 
checked)  The elements of etl parallel mel so each edge will have the same 
index in both.   In this version, edges are assumed to be 'non-renewal' meaning
 they must be described by a single interval.  But multiple edges on single node 
 pair are permitted.  WARNING: currently the etl list is an R datastrcture, and 
 the rest of the object is backed in C.  This means you have to be VERY careful 
 to pass the modified netowrk back.  

Attribute changes are implemented with a  system closer to the change-list than
 the valid interval format.  Dynamic attributes are stored on the vertex 
 attribute list alongside static attributes. They are stored as a two-column 
 matrix in which the first column contains the value, and the second the time 
 at which the attribute should take on that value and distinguished from static 
 attributes by appearing in the network level 'dynam.attr.names' list.  Values 
 must be in ascending order by time, but it is up to the user to insert them in order. 
 \code{\preformatted{
> myGraph$val[[10]]    #the dynamic color attribute for node 10 on the vertex attribute list of myGraph
$color
     [,1]          [,2]
[1,] "blue"    "8" 
[2,] "green"  "9" 
[3,] "red"      "10" 
} }
So for the example above the value of 'color' for node 10 is undefined for all 
times before 8, 'blue' from 8 to 9, 'green' from 9 to 10, and is 'red' for all
 values after time 10.   Although it should be possible to store arbitrary 
 objects in this format, the implementation is currently limited to text strings.

  The same approach is applied to attach dynamic attributes to edges.    
}
\value{
  A \code{\link{dynamicnetwork}}
}
\references{  }
\author{ Skye Bender-deMoll \email{skyebend@skyeome.net}}

\seealso{ documentation for the network package } 
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
#now add some attributes
dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time=8,v=10)
dyn <- set.dynamic.vertex.attribute(dyn, "color","green",valid.time=9,v=10)
dyn <- set.dynamic.vertex.attribute(dyn, "color","blue",valid.time=10,v=10)
print(dyn);

}
\keyword{ classes }% at least one, from doc/KEYWORDS
\keyword{ graphs }% __ONLY ONE__ keyword per line
