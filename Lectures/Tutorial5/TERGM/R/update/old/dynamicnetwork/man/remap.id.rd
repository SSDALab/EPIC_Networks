\name{remap.id}
\alias{remap.id}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ translates an id to an id in a subsample }
\description{
  returns the index of the id in the subsample, effectively translating it to an
  id usable as a vertex id
}
\usage{
remap.id(id, subsample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{id}{ unique id that needs to be translated to a vertex id }
  \item{subsample}{ list of all unique ids }
}
\details{
  provides an easy way to map back and forth between a vertex id (that must be an
  integer suitable for a matrix index) and any other unique id labeling system
  used in input data.  
}
\value{
  an integer giving a vertex id that is an index number on the list of ids on the 
  passed list.  
}

\author{ skyebend@skyeome.net }

\examples{

#list of unique ids using some other naming scheme
idlist <- c("a","b","c","d");
#get a vertex id
remap.id("c",idlist);    #returns 3

#chech that it translates back..
idlist[remap.id("c",idlist)];     # yup, returns "c"
}
\keyword{ graphs }

