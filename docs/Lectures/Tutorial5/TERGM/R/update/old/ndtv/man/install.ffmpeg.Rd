%  File man/install.ffmpeg.Rd in package ndtv, part of the Statnet suite
%  of packages for network analysis, http://statnet.org .
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) at
%  http://statnet.org/attribution
%
%  Copyright 2003-2012 Statnet Commons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{install.ffmpeg}
\alias{install.ffmpeg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Instructions for installing ffmpeg on various platforms
}
\description{
The animation package uses ffmpeg to export movies into video formats. This internal function doesn't actually install the ffmpeg library, it just gives instructions on how to do the installation
}
\usage{
install.ffmpeg()
}
%- maybe also 'usage' for other objects documented here.
\details{
On windows: 

download the recent 'static' build from  http://ffmpeg.zeranoe.com/builds/

downloads are compressed with 7zip, so you may need to first install a 7zip decompression program.

decompress the package and store contents on your computer (probably in Program Files)

Edit your system path variable to include the path to the directory containing ffmpeg.exe

On Mac

Download most recent build from http://www.evermeet.cx/ffmpeg/

Binary files are compressed with 7zip so may need to install an unarchiving utility:http://wakaba.c3.cx/s/apps/unarchiver.html

Copy ffmpeg to /usr/local/bin/ffmpeg
}
\value{
On winddows:Will open a web browser window to the ffmpeg website and give instructions how to open this help file. 
}
\references{
http://ffmpeg.zeranoe.com/
}
