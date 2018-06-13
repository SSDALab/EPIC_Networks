library(knitr)
ifiles<-dir("Lab3")
ifiles<-ifiles[grep(".Rmd",ifiles)]
ifiles<-paste("Lab3/",ifiles,sep="")
ofiles<-paste(sapply(strsplit(ifiles,"\\."),"[[",1),".R",sep="")

for(i in 1:length(ifiles)){
purl(ifiles[i], output = ofiles[i], documentation = 2)
}
