\name{hiliteColor}
\alias{hiliteColor}
\title{ Maps boolean values to red and black color names}
\description{
  Used as a color function for taking a vector of booleans (like infection
  status) and create a vector of color names to be used in a plot. 
}
\usage{
hiliteColor(values)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{ vector of boolean values }
}
\details{
  Replaces all TRUE values with  the string 'Red', all FALSE values with the 
  string 'Black'.
}
\value{
  character vector of color names
}
\author{ skyebend@skyeome.net }

\examples{

## The function is currently defined as
function (values) 
{
    values <- replace(values, values == TRUE, "Red")
    values <- replace(values, values == FALSE, "Black")
    return(values)
  }
}
\keyword{ manip }
\keyword{ color }

