\name{attribute.activity.functions}
\alias{activate.vertex.attribute}
\alias{get.vertex.attribute.active}
\alias{activate.edge.attribute}
\alias{activate.edge.value}
% \alias{get.edge.attribute.active}
\alias{get.edge.value.active}
\alias{activate.network.attribute}
\alias{get.network.attribute.active}
\alias{deactivate.edge.attribute}
\alias{deactivate.vertex.attribute}
\alias{deactivate.network.attribute}
\alias{list.vertex.attributes.active}
\alias{list.edge.values.active}
\alias{list.network.attributes.active}
\title{
Functions to activate and query dynamic attributes on vertices, edges, and networks
}
\description{
These functions set, query and modify attributes of networkDynamic objects used to represent values which change over time. These dynamic or "temporally extended attributes" are stored as standard attributes with a specially defined structure.
}
\usage{
activate.vertex.attribute(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, v = 1:network.size(x), dynamic.only = FALSE)

activate.edge.attribute(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, e = seq_len(network.edgecount(x)),   dynamic.only = FALSE)
    
activate.edge.value(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, e = seq_len(network.edgecount(x)),   dynamic.only = FALSE)
    
activate.network.attribute(x, prefix, value, onset = NULL, terminus = NULL, 
    length = NULL, at = NULL, dynamic.only = FALSE)    
    
get.vertex.attribute.active(x, prefix, onset = NULL, terminus = NULL, length =
                 NULL, at = NULL, rule = c("any", "all"), na.omit =
                 FALSE, null.na = TRUE, active.default = TRUE,
                 dynamic.only = FALSE, require.active = FALSE,
                 return.tea = FALSE, unlist = TRUE)   
    
get.edge.value.active(x, prefix, onset=NULL, terminus=NULL,length=NULL, at=NULL,
                rule = c("any", "all"), active.default = TRUE, dynamic.only=FALSE, 
                require.active=FALSE,return.tea=FALSE,unlist=TRUE) 

get.network.attribute.active(x, prefix, onset=NULL, terminus=NULL,length=NULL, 
                at=NULL, rule = c("any", "all"), dynamic.only=FALSE, 
                return.tea=FALSE,unlist=TRUE) 
                
deactivate.vertex.attribute (x, prefix, onset=NULL, terminus=NULL,
                length=NULL,at=NULL, v=1:network.size(x), dynamic.only=FALSE)
                 
deactivate.edge.attribute (x, prefix, onset=NULL, terminus=NULL,length=NULL,
                at=NULL,e=seq_len(network.edgecount(x)), dynamic.only=FALSE)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 a \code{networkDynamic} object
}
  \item{prefix}{
 character string for the name of the attribute to be stored (will be stored as prefix.active)
}

  \item{value}{
 an object that is the value of the attribute to be stored. If \code{value} is a \code{list}, it is assumed that each element corresponds to an index in \code{v}.
}
  \item{onset}{
an optional vector of time-points that specifies the starts of the interval(s) at which the \code{value} argument should be considered active.
}
  \item{terminus}{
 an optional vector of time-points that specifies the ends of the interval(s) at which the \code{value} argument should be considered active. 
 }
 \item{length}{
 an optional vector of interval lengths for the interval(s). This must be accompanied by one of \code{onset} or \code{terminus}.
 }
 \item{at}{
 optional, one or more time points to be activated
 }
  \item{v}{
optional, one or more IDs indicating which vertices should have the TEA attribute added (default is all vertices)
}

 \item{e}{
 optional, one or more edge IDs indicating which edge should have the TEA attribute added (default is all edges)
 }
 \item{dynamic.only}{
  If dynamic.only==TRUE, only TEAs are considered; otherwise, non-TEA attributes are also employed (with activity as defined by active.default). 
 }
 
 \item{require.active}{
   logical. If TRUE, NA will be returned instead of an attribute value if the associated vertex or edge is inactive for the query period. When vertex or edge activity has not be explicitly set, the state is controlled by \code{active.default}.
 }
 
 \item{active.default}{
   logical; should underlying vertices or edges without an activity attribute be regarded as active by default?
 }

 \item{na.omit}{
  logical; should values from missing vertices/edges be removed? NOT IMPL
 }
 
 \item{null.na}{
   logical; should NULL values be replaced with NAs? NOT IMPL
 }
 
 \item{rule}{
   a character string indicating the definition of "active" for this query: \code{any} (default) to define active as any active attribute spell during the query interval, or \code{all} to define active as being active over the entire interval.
 }
 
 \item{return.tea}{
   logical; if \code{return.tea=TRUE}, will return an (appropriately truncated) activity attribute- a list with the first element a list of values and the second element the spell matrix of corresponding spells matching according to \code{rule}. Also sets \code{unlist=FALSE} and \code{dynamic.only=TRUE}. If \code{return.tea=FALSE}(default) will return a single matching value. If multiple values match, will return earliest and give warning. 
 }
 
 \item{unlist}{
   logical; should retrieved attributes be unlisted prior to being returned? Setting to FALSE is necessary to correctly fetch list objects stored as attributes.
 }


}
\details{
These functions provide easy ways to work with temporally extended attributes, making it possible to store attributes on a dynamic network that should be represented as changing values and different points in time. Because the serve as wrappers for get.*.attribute and set.*.attribute, many of the arguments are directly passed through. 

The activate.*.attribute methods act as a cross between  activate.*  set.*.attribute methods.  They are used to activate an attribute for a given spell, and in so doing set a value for that spell.  The corresponding deactivate methods are more straightforward, deactivating the attribute over a set interval (and removing any spells/values as needed).

A dynamic or temporally extended attribute is an edge, vertex, or network attribute satisfying the following properties:

 1. Its name consists of an arbitrary prefix, together with the suffix ".active".
 2. Its value consists of a two-element list, whose respective contents must be maintained in order as follows:
	    a. A list of value entries, such that the ith list element is the value of the attribute for the ith spell in the associated activity matrix.  An attribute not active at any given point in time is defined as having a value of NA (but the reverse is not true -- an active attribute can also take a value of NA).
	    b. A two-column numeric matrix, conforming to the specifications for activity attributes in the networkDynamic package.  Every active spell indicated in the activity matrix must correspond to exactly one entry of the associated value list, and these must be maintained in order (i.e., the value of the attribute for the ith active spell is the ith element in the value list).
      
There are several ways to specify the activity spell, and the general syntax rules are described at \code{\link{activity.attribute}}.  Activity can be set for a single time point, using either \code{at}, or by setting onset=terminus.  Activity can be set for an interval [onset,terminus), using a valid combination of the \code{onset}, \code{terminus} and \code{length} attributes.

  
This allows for a wide range of options when specifying spells, but a correct specification must use only one of these forms:
\preformatted{
  at
  onset and terminus 
  onset and length
  terminus and length
  or, you can provide no timing information 
}

If provided with no timing information, the element is activated/deactivated from \code{-Inf} to \code{Inf}.   
The specified interval spans the period from the onset (inclusive) to the terminus (exclusive), so [onset,terminus). For more details, see \code{\link{activate}}

\code{activate.edge.value} is a wrapper for \code{activate.edge.attribute}. 

     
}
\value{
The get methods return the attribute object appropriate for the time range specified query spell parameters.  If query results in multiple values and \code{return.tea=FALSE} only the first (earliest) is returned along with a warning. Note that \code{get.edge.value.active} returns \code{NULL} if no attributes names match, where \code{get.vertex.attribute.active} returns \code{NA}s. 
}
\references{
See statnet internal TEA spec documents \url{https://statnet.csde.washington.edu/trac/wiki/TemporallyExtendedAttributes}
}
\author{
skyebend, ayn, carter, pavel, martina, steve
}
\note{
WARNING! These are early draft implementations of the functions which may not fully meet spec.  They are probably not efficient. They might explode. Use at your own risk! :-)

Attribute activity is only checked against vertex and edge activity during fetching if \code{require.active=TRUE}.  Otherwise the attributes for a vertex can take values (be active) when the vertex itself is inactive. Likewise for edges. 
}


\seealso{
See Also as \code{\link{set.vertex.attribute}}, \code{\link{get.vertex.attribute}},\code{\link{activate.vertices}}, \code{\link{activity.attribute}}, \code{\link{activate}}
}
\examples{
test<-network.initialize(5)
test<-activate.vertex.attribute(test,"letter","a",onset=0,terminus=1)
test<-activate.vertex.attribute(test,"letter","b",onset=1,terminus=2)
test<-activate.vertex.attribute(test,"letter","c",onset=2,terminus=3)
get.vertex.attribute.active(test,"letter",onset=2,terminus=3)

test[1,2]<-1
test[2,3]<-1
test<-activate.edge.attribute(test,"number",1,onset=0,terminus=2)
test<-activate.edge.attribute(test,"number",5,onset=2,terminus=5)
get.edge.value.active(test,"number",onset=3,terminus=4)

# add a more complex value as a network attribute
test <- activate.network.attribute(test,"alist",list("a","b"),onset=1,terminus=2)
test <- activate.network.attribute(test,"alist",list("c","d"),onset=2,terminus=3)
get.network.attribute.active(test,"alist",at=2.5,unlist=FALSE)

}

