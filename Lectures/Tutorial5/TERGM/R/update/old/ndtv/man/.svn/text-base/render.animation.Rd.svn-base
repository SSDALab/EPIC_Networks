%  File man/render.animation.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2012 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{render.animation}
\alias{render.animation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Render animations of networkDynamic objects as movies in various formats
}
\description{
Takes a network object which describes a sequence of changes in properties of a network and graphically renders it out as a sequence of images to create an animation . Normally the coordinates determining the vertex positions at specified timepoints should have been calculated and stored in the network object, along with the \code{slice.par} list of parameters describing when and how the network should be divided up in time. If the coordinate data is not found, \code{\link{compute.animation}} will be called with default arguments. The rendering of each frame is drawn by \link{plot.network} and most arguments are supported and are passed through to control the details of rendering. The rendered images are stored using the \code{\link{animation}} package and be played in the plot window (\code{\link{ani.replay}}) or saved to a movie file with \code{\link{saveVideo}}.
}
\usage{
render.animation(net, render.par = list(tween.frames = 10, show.time = TRUE, show.stats = NULL), verbose=TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
The networkDynamic object to be rendered, usually with pre-computed vertex positions. 
}
  \item{render.par}{
Named list of parameters to specify the behavior of the animation.
  \itemize{
  \item \code{tween.frames} the number of interpolated frames to generate between each calculated network layout. 
  \item \code{show.time} If true, labels the plot with onset and terminus time for each slice. 
  \item \code{show.stats} NULL, or a string containing a formula to be passed to summary.stergm to display the network statistics for the current slice on the plot

  }
}

  \item{...}{
 Other parameters to control network rendering. See \code{link{plot.network}. }
}
  
  \item{verbose}{
 If TRUE, update text will be printed to the console to report the status of the rendering process.
}
}
\details{

Most of the network plotting arguments are passed directly to \code{\link{plot.network.default}}.  There is very limited support for dynamic attributes at this time.  

If no \code{slice.par} network attribute is found to define the time range to render it will make one up using the smallest and largest non-Inf time values and unit-length non-overlapping time steps. 

If no dynamic coordinate information has been stored, compute \code{\link{compute.animation}} will be called with default values to try to do the layout before giving up. 
}

\note{
A few of the network drawing arguments have slightly different interpretations thatn their \link{plot.network} equivilents:
\itemize{
\item \code{xlab} will be used to display time and network statistics if those render.par parameters are set
\item \code{xlim} and \code{ylim} will be set using max and min observed coordinate values so that all network slices will appear on the plot

\item \code{label} if not set explicitly, will default to the vertex names for the appropriate slice network. 
}
}

\value{
The recorded plots are stored as a list in \code{.ani.env$.images} (this will probably change in the future)
}
\references{
Skye Bender-deMoll and McFarland, Daniel A. (2006) The Art and Science of Dynamic Network Visualization. \emph{Journal of Social Structure. Volume 7, Number 2} \url{http://www.cmu.edu/joss/content/articles/volume7/deMollMcFarland/}
}
\author{
Skye Bender-deMoll, and the statnet team. 
}
\note{
This is still a draft implementation and most of the package. Most specifics (including the function name) are likely to change.
}


\seealso{
\link{compute.animation},\link{ani.record}, \link{plot.network} and the package vignette \code{vignette('ndtv')}
}
\examples{

require(ndtv)
# trivial example

triangle <- network.initialize(3) # create a toy network
add.edge(triangle,1,2)
# add an edge between vertices 1 and 2
add.edge(triangle,2,3)
# add a more edges
activate.edges(triangle,at=1) # turn on all edges at time 1 only
activate.edges(triangle,onset=2, terminus=3,
e=get.edgeIDs(triangle,v=1,alter=2))
add.edges.active(triangle,onset=4, length=2,tail=3,head=1)
render.animation(triangle)
ani.replay() 

# simulation based example
require(tergm)
# load in example data, results of a basic stergm sim
data(stergm.sim.1)

# define the number of inbetween frames and a formula for stats to display
render.par<-list(tween.frames=5,show.time=TRUE,
                show.stats="~edges+gwesp(0,fixed=TRUE)")
                
# render the movie, with labels, smaller vertecies, etc
render.animation(stergm.sim.1,render.par=render.par,
                 edge.col="darkgray",displaylabels=TRUE,
                 label.cex=.6,label.col="blue")
                 
# preview the movie in the plot window
ani.replay()     

# save the movie as mp4 compressed video (if FFMPEG installed)
saveVideo(ani.replay(),video.name="stergm.sim.1.mp4", 
                    other.opts="-b 5000k",clean=TRUE)

}

