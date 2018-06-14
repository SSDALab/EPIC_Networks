%  File man/layout.distance.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2012 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{layout.distance}
\alias{layout.distance}
\title{
Provides a default way to convert a network into a set of euclidian distances suitable for MDS-style layout optimization.
}
\description{
Computes a geodesic path distance matrix for a network after symmetrizing, replacing Inf values with \code{default.dist} 
}
\usage{
layout.distance(net, default.dist = NULL)
}
\arguments{
  \item{net}{
The network that the distance matrix should be computed for
}
  \item{default.dist}{
An (optional) value to be used to replace undefined values created by isolates and disconnected components. 
}
}
\details{
If no \code{default.dist} is provided the value \code{sqrt(network.size(net))} will be used. 
}
\value{
A distance matrix assumed to be appropriate for the network.
}

\author{
Skye Bender-deMoll
}


