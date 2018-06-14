# Let's calculate k-cores (using total degree) on Coleman's high school data
gplot(coleman,g=1)                           # Two matrices in the Coleman stack
kc1<-kcores(coleman[1,,])                    # Calculate kcores for fall
kc2<-kcores(coleman[2,,])                    # Calculate kcores for spring

#Examine the core distribution
table(kc1)
table(kc2)

gplot.spatial<-function(x,coord,bb,list.edges,list.vertex){
	#require(geosphere)
	plot.new()
	plot.window(xlim=bb[1,],ylim=bb[2,],asp=1)
	axis(1,cex.axis=.5,lwd.ticks=.5,las=1)
	axis(2,cex.axis=.5,lwd.ticks=.5)
	box()
	x<-as.matrix.network(x,"edgelist")
	#for(i in 1:NROW(x)){
	do.call("lines",c(list(x=rbind(coord[x[,1],],coord[x[,2],])),list.edges))      
	#}     
	do.call("points",c(list(x=coord),list.vertex))
	#title(xlab="meters (m)",ylab="meters (m)")          
}


plot.kcor<-function(net,coord,bb,cores){
gplot.spatial(net,coord,bb,list.vertex=list(col=heat.colors(max(cores)+1)[cores+1]),pch=19,cex=.0001),list.edges=list(col=rgb(0,0,0,.3),lwd=.3))
}

png(paste(name,".png",sep=""),width=1600,height=1200,res=300,bg="white")
plot.kcor()
dev.off()

gplot(net,vertex.col=heat.colors(max(kc1)+1)[kc1+1])
gplot(coleman[1,kc1>1,kc1>1],vertex.col=
    heat.colors(max(kc1[kc1>1])+1)[kc1[kc1>1]+1])        # 2-core
gplot(coleman[1,kc1>2,kc1>2],vertex.col=
    heat.colors(max(kc1[kc1>2])+1)[kc1[kc1>2]+1])        # 3-core
gplot(coleman[1,kc1>3,kc1>3],vertex.col=
    heat.colors(max(kc1[kc1>3])+1)[kc1[kc1>3]+1])        # 4-core
	
}


# Visualize the core structure
gplot(coleman[1,,],vertex.col=heat.colors(max(kc1)+1)[kc1+1])
gplot(coleman[1,kc1>1,kc1>1],vertex.col=
    heat.colors(max(kc1[kc1>1])+1)[kc1[kc1>1]+1])        # 2-core
gplot(coleman[1,kc1>2,kc1>2],vertex.col=
    heat.colors(max(kc1[kc1>2])+1)[kc1[kc1>2]+1])        # 3-core
gplot(coleman[1,kc1>3,kc1>3],vertex.col=
    heat.colors(max(kc1[kc1>3])+1)[kc1[kc1>3]+1])        # 4-core

# Visualize the corresponding shells (note: don't confuse with the cores!)
gplot(coleman[1,kc1==2,kc1==2],vertex.col=
    heat.colors(max(kc1[kc1==2])+1)[kc1[kc1==2]+1])        # 2-shell
gplot(coleman[1,kc1==3,kc1==3],vertex.col=
    heat.colors(max(kc1[kc1==3])+1)[kc1[kc1==3]+1])        # 3-shell
gplot(coleman[1,kc1==4,kc1==4],vertex.col=
    heat.colors(max(kc1[kc1==4])+1)[kc1[kc1==4]+1])        # 4-shell
