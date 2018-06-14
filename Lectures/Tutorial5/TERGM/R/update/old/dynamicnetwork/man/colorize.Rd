\name{colorize}
\alias{colorize}
\title{ Function to map colors to variable values }
\description{
   Creates an arbitrary mapping from a set of variable values to a set of 
  color names which can be read by SoNIA.  Used to give default coloring. 
  Can also accept color funcions or user mappings as input
}
\usage{
colorize(x, values = unique(x), colors = rainbow(length(values)),
truncateAlpha=TRUE,
 printLegend = TRUE, plotLegend = FALSE, map.na = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ a vector containing data elements that should be mapped to colors }
  \item{values}{ the unique values of x (defaults to unique(x)) }
  \item{truncateAlpha}{ if true, the hex color codes will be truncated to trim
  off the last two alpha digits.  Warning: don't use this option with color names. }
  \item{colors}{ color names or values associated with the elements of values }
  \item{printLegend}{ if true, prints a text legend.  Only useful if input is named colors }
  \item{plotLegend}{ if true, puts up a plot window with colors and values }
  \item{map.na} {if true, NA values will be converted to the text `NA' and assigned a color}
}
\details{
      if \code{values} is shorter than \code{colors}, uses only the first elements
      of colors.  
}
\value{
  x, but with each unique value replaced with a color, usually expressed as a hex code. 
  As the hex codes in R 2.4 now included an extra alpha which throws everying all to hell, 
  the option \code{truncateAlpha} is included to remove the last two chars of the hex.  
}

\author{ skyebend@skyeome.net }
\seealso{ \code{\link{colors}},\code{\link{palette}},\code{\link{topo.colors}}}
\examples{

#use the default palette, useful if no more than 8 values
colorize(c("a","b","c","d"),colors=palette());
#use specific named colors
colorize(c("a","b","c","d"),colors=c("red","green","blue","black"));

## The function is currently defined as
function(x,values=unique(x),
colors=rainbow(length(values)),
printLegend=TRUE,plotLegend=FALSE){
#check that values and colors are the same length
if (length(colors) < length(values)){
  stop("lengths of colors and values vectors are not the same, each value must match with a color");
  }

 for (colorNum in 1:length(colors)){
   x <- replace(x,x==values[colorNum],colors[colorNum]);
 }
 #change to rgb values to avoid bug where 2.4 version addes alpha code to hex colors
 x <- col2rgb(x);
 if(printLegend){
   print(t(rbind(values,colors[1:length(values)])));
 }
 if(plotLegend){
     plot(rep(0,length(values)),1:length(values),col=colors,cex=5,pch=19);
   text(rep(0,length(values)),1:length(values),values);
 }
 return(x);
  }
}
\keyword{ manip }

