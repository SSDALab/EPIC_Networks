library(katrina)
data(katrina.combined)
data(katrina.bydate)
temp<-katrina.bydate[[9]]
temp<-add.vertices(temp,(network.size(katrina.combined)-network.size(temp)))

coord<-plot(temp,vertex.cex=.7)
nam<-temp%v%"vertex.names"
m<-match(katrina.bydate[[9]]%v%"vertex.names",nam)
par(mfrow=c(1,2),mar=c(0, 0, 1, 0) + 0.1)
plot(katrina.bydate[[9]],vertex.cex=.7,coord=coord[m,])
title("Day 9")
plot(temp,vertex.cex=.7,coord=coord)
title("Day 9 with Fixed Population")