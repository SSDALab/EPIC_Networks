library(knitr)
ifiles<-paste("Tutorial",1:6,".Rmd",sep="")
ofiles<-paste("Rfiles/Tutorial",1:6,".R",sep="")

for(i in 1:length(ifiles)){
purl(ifiles[i], output = ofiles[i], documentation = 2)
}
