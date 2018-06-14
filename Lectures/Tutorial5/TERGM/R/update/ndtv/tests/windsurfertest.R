require(networkDynamic)
require(network)
require(ndtv)
require(sna)
require(animation)
data(windsurfers)




#daily
slice.par<-list(start=0,end=31,interval=1, aggregate.dur=1,rule="any")
windsurfers <-compute.animation(windsurfers,slice.par=slice.par,animation.mode="MDSJ",default.dist=3)
render.animation(windsurfers,render.par=list(tween.frames=25,show.time=T),vertex.col="group1",edge.col="darkgray",displaylabels=T,label.cex=.6,label.col="blue")
saveVideo(ani.replay(),video.name="windsurfers_dailyDraft.mp4", other.opts="-b 1000k")


#wekly
slice.par<-list(start=0,end=31,interval=1, aggregate.dur=7,rule="any")
windsurfers <-compute.animation(windsurfers,slice.par=slice.par,animation.mode="MDSJ",default.dist=3)
render.animation(windsurfers,render.par=list(tween.frames=25,show.time=T),vertex.col="group1",edge.col="darkgray",displaylabels=T,label.cex=.6,label.col="blue")
saveVideo(ani.replay(),video.name="windsurfers_weeklyDraft.mp4", other.opts="-b 1000k")


#make a version of the windsurfers where the nodes stay around
deactivate.vertices(windsurfers)
activate.vertices(windsurfers,onset=0,terminus=24)
activate.vertices(windsurfers,onset=25,terminus=31)

#daily
slice.par<-list(start=0,end=31,interval=1, aggregate.dur=1,rule="any")
windsurfers <-compute.animation(windsurfers,slice.par=slice.par,animation.mode="MDSJ",default.dist=3)
render.animation(windsurfers,render.par=list(tween.frames=25,show.time=T),vertex.col="group1",edge.col="darkgray",displaylabels=T,label.cex=.6,label.col="blue")
saveVideo(ani.replay(),video.name="windsurfers_dailyAllNodes.mp4", other.opts="-b 1000k")
