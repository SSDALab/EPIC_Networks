library(knitr)
ifiles<-dir("Lab2")
ifiles<-ifiles[grep(".Rmd",ifiles)]
o<-sapply(strsplit(ifiles,"\\."),"[[",1)
ofiles<-paste("lab2/",o,".R",sep="")

for(i in 1:length(ifiles)){
purl(paste("Lab2/",ifiles[i],sep=""), output = ofiles[i], documentation = 2)
}
