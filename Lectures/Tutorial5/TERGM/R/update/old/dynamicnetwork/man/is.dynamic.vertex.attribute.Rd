\name{is.dynamic.vertex.attribute}
\alias{is.dynamic.vertex.attribute}
\alias{is.dynamic.edge.attribute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Checks if an attribute is dynamic }
\description{
  Checks if the passed attribute name is a dynamic (time varying) vertex
  attribute in the passed dynamic network. 
}
\usage{
is.dynamic.vertex.attribute(network, attrname)
is.dynamic.edge.attribute(network, attrname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{network}{ dynamicnetwork to look in for vertex attributes }
  \item{attrname}{ string give name of attribute to look for}
}
\details{
  checks if the attrname is on the \code{dynam.attr.names} network property for 
  vertex, or  \code{dyn.edge.attr.names}  for edges. 
}
\value{
  TRUE if the attrname is the name of a dynamic attribute, FALSE otherwise
}
\author{ skyebend }

\examples{
 test <- as.dynamic(network.initialize(5));
 #give vertex 4 a dynamic attribute
test <- set.dynamic.vertex.attribute(test, "color","blue",valid.time=8,v=4);
#also give it a static attribute
test <- set.vertex.attribute(test,"skill","good",v=4);
is.dynamic.vertex.attribute(test,"color");     #true
is.dynamic.vertex.attribute(test,"fred");       #false, not an attribute
is.dynamic.vertex.attribute(test,"skill"); #false, it is static
}

\keyword{ graphs }% at least one, from doc/KEYWORDS

