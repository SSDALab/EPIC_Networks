################################################################################
#
#  Network Methods Lecture 2 - R/Statnet Component
#
#  SOC 280 - Analysis of Social Network Data
#  Carter T. Butts, University of Califorina, Irvine
#  Spring Quarter, 2009
#
#  (Additional credits to the statnet team, including S.M. Goodreau, M.S.
#   Handcock, D.R. Hunter, M. Morris, and yours truly.)
#
#  Modified by Zack Almquist, June 11, 2018
################################################################################
#
#-A reminder on getting started....---------------------------------------------
#
library(sna)                      # Load the sna library
help.start()                      # If not done already...walk through the
                                  # various sna pages
library(help="sna")               # See also this for a list

#For more information....
?help.start
?library
?sna

#  library(devtools)
#  install_githup("zalmquist/networkMethods")

library(networkMethods)
data(contig_1993)
data(mids_1993)
#
#-Network visualization with gplot----------------------------------------------
#
# Begin by plotting contiguity among nations in 1993 (from the Correlates of 
# War project)
gplot(contig_1993)                              # The default visualization
gplot(contig_1993, usearrows=FALSE)             # Turn off arrows manually
gplot(contig_1993, gmode="graph")               # Can also tell gplot the data
                                                # is undirected

# We can add labels to the vertices - network.vertex.names reports them
gplot(contig_1993, gmode="graph", label=network.vertex.names(contig_1993))

# This plot is too large/dense for the default settings to work.  Let's refine
# them.
gplot(contig_1993, gmode="graph", boxed.labels=FALSE, label.cex=0.5, 
    label.col=4, label=network.vertex.names(contig_1993)) # Shrink labels,
                                                         # remove boxes, recolor

# Here's an example of directed data - militarized interstate disputes (MIDs) 
# for 1993
gplot(mids_1993, boxed.labels=FALSE, label.cex=0.5, label.col=4,
   label=network.vertex.names(mids_1993))       # Basic display, with labels

# All those isolates can get in the way - we can suppress them using 
# displayisolates
gplot(mids_1993, displayisolates=FALSE, boxed.labels=FALSE, label.cex=0.5, 
    label.col=4,label=network.vertex.names(mids_1993))

# The default layout algorithm is that of Frutchterman-Reingold (1991), can use 
# others
gplot(mids_1993, displayisolates=FALSE, boxed.labels=FALSE, label.cex=0.5, 
    label.col=4,label=network.vertex.names(mids_1993), 
    mode="circle")   # The infamous circle
gplot(mids_1993, displayisolates=FALSE, boxed.labels=FALSE, label.cex=0.5,
   label.col=4,label=network.vertex.names(mids_1993), 
   mode="mds")      # MDS of position similarity

# When a layout is generated, the results can be saved for later reuse:
coords <- gplot(contig_1993)                  # Capture the magic of the moment
coords                                        # Show the vertex coordinates

#Saved (or a priori) layouts can be used via the coord argument:
gplot(mids_1993, boxed.labels=FALSE, label.cex=0.5, label.col=4, coord=coords,
   label=network.vertex.names(mids_1993))        # Relive the magic

# When the default settings are insufficient, interactive mode allows for 
# tweaking
coords <- gplot(contig_1993, interactive=TRUE)   # Modify and save
gplot(contig_1993, coord=coords, gmode="graph")  # Should reproduce the modified
                                                 # layout

#For more information....
?gplot
?gplot.layout

#
#Three-dimensional visualization with gplot3d (requires the rgl package)--------
#
# Note: if you haven't done so, you can install the rgl package by typing
# install.packages("rgl") at the command prompt (assuming you are online).
# If that package is not installed, you'll get a boring error message instead
# of exciting visualization.

gplot3d(contig_1993, label=network.vertex.names(contig_1993))   # Experience 
                                                                # the future!
# Other layouts are possible here, too:
gplot3d(contig_1993, label=network.vertex.names(contig_1993),mode="kamadakawai")

#For more information....
?gplot3d
?gplot3d.layout
