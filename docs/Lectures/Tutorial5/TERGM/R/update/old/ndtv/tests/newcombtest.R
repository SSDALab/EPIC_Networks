require(networkDynamic)
require(network)
require(ndtv)
require(sna)
require(animation)
data(newcomb)
frats <-ndtv:::as.dynamic.network.panel(newcomb)

slice.par<-list(start=0,end=14,interval=1, aggregate.dur=1,rule="any")
frats <-compute.animation(frats,slice.par=slice.par,animation.mode="MDSJ")
render.animation(frats,render.par=list(tween.frames=25,show.time=T),edge.col="darkgray",displaylabels=T,label.cex=.6,label.col="blue")
saveVideo(ani.replay(),video.name="newcombBasic.mp4", other.opts="-b 1000k",clean=TRUE) #probably need to set this to not open video window by default
