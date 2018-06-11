library(dplyr)
library(tidyr)
library(foreign)
library(sna)
library(network)

setwd("/Users/almquist/Documents/Classes/Workshops/ERGM/Slides/data/HealthNetworks")

# http://www.icpsr.umich.edu/icpsrweb/NAHDAP/studies/22140
#data<-read.table("/Users/almquist/Downloads/ICPSR_22140/DS0001/22140-0001-Data.tsv")


data<-read.spss("/Users/almquist/Documents/Classes/Workshops/ERGM/Slides/data/HealthNetworks/ICPSR_22140/DS0001/22140-0001-Data.sav")

data2<-read.spss("/Users/almquist/Documents/Classes/Workshops/ERGM/Slides/data/HealthNetworks/ICPSR_22140/DS0002/22140-0002-Data.sav")

data3<-read.spss("/Users/almquist/Documents/Classes/Workshops/ERGM/Slides/data/HealthNetworks/ICPSR_22140/DS0003/22140-0003-Data.sav")

head(as.data.frame(data2)[,1:6])
head(as.data.frame(data3)[,1:6])

ego<-as.data.frame(data2)[,c("rid","id1","id2","dyadkey","dyadtype","tietype","race","race1","race2", "ethn","ethn1","ethn2","sex","sex1","sex2","age","age1","age2","studynum","streets" ,"streets1","streets2")]

alterAlter<-as.data.frame(data3)[,c("rid","id1","id2","dyadkey","dyadtype","tietype","race","race1","race2", "ethn","ethn1","ethn2","sex","sex1","sex2","age","age1","age2","studynum","streets" ,"streets1","streets2")]

CS_P90_ego<-filter(ego,studynum=="CoSprings Project90" & tietype == "Sexual") #Needle
CS_P90_alter<-filter(alterAlter,studynum=="CoSprings Project90" & tietype == "Sexual")
head(CS_P90_ego)

ego_EL<-CS_P90_ego[,c("rid","id2")]
temp<-CS_P90_alter[,c("id1","id2")]
colnames(temp)<-c("rid","id2")
ego_EL<-rbind(ego_EL,temp)
ego_EL[,1]<-as.numeric(as.character(ego_EL[,1]))
ego_EL[,2]<-as.numeric(as.character(ego_EL[,2]))
ego_EL<-unique(as.matrix(ego_EL))
ego_EL<-cbind(ego_EL,rep(1,NROW(ego_EL)))
colnames(ego_EL)<-c("snd", "rec","val")
#ego_EL<-as.edgelist.sna(ego_EL)
attr(ego_EL,"n")<-length(unique(ego_EL[,1],ego_EL[,2]))
attr(ego_EL,"vnames")<-unique(ego_EL[,1],ego_EL[,2])
is.edgelist.sna(ego_EL)
mat<-as.sociomatrix.sna(network(ego_EL[,1:2],directed=FALSE))

png("CoSpringsEgo.png")
gplot(mat)
dev.off()

sociomatrix<-function(x){
  n<-attr(x,"n")
  vnam<-attr(x,"vnam")
    g <- matrix(0, n, n)
                if (NROW(x) > 0)
                  g[x[, 1:2, drop = FALSE]] <- x[, 3]
                rownames(g) <- vnam
                colnames(g) <- vnam
    g
}

buildSN<-function(alter){

mat<-alter
i <- sapply(mat, is.factor)
mat[i] <- lapply(mat[i], as.character)
i <- sapply(mat, is.numeric)
mat[i] <- lapply(mat[i], as.character)


covar<-
data.frame(
stack(mat[,c("rid","id1","id2")])[,1],
stack(mat[,c("race","race1","race2")])[,1],
stack(mat[,c("ethn","ethn1","ethn2")])[,1],
stack(mat[,c("sex","sex1","sex2")])[,1],
stack(mat[,c("age","age1","age2")])[,1],
stringsAsFactors = FALSE
)
names(covar)<-c("id","race","ethn","sex","age")
covar$age<-as.numeric(covar$age)
covar_agg<-aggregate(covar[,"age"],by=covar[,c("id","race","ethn","sex")],FUN="min")
names(covar_agg)[5]<-"age"
#head(covar_agg)
attr<-covar_agg

temp1<-alter[,c("rid","id1")]
temp2<-alter[,c("rid","id2")]
temp3<-alter[,c("id1","id2")]
colnames(temp1)<-c("one","two")
colnames(temp2)<-c("one","two")
colnames(temp3)<-c("one","two")

temp_EL<-rbind(temp1,temp2,temp3)
nam<-unique(c(temp_EL[,1],temp_EL[,2]))
index1<-match(temp_EL[,1],nam)
index2<-match(temp_EL[,2],nam)
EL<-cbind(index1,index2,rep(1,length(index1)))
colnames(EL)<-c("snd", "rec","val")
attr(EL,"n")<-length(nam)
attr(EL,"vnames")<-as.character(nam)
if(!is.edgelist.sna(EL)){return("BAD!")}
#EL_sna<-as.edgelist.sna(EL)
net<-network(sociomatrix(EL),directed=FALSE)

#if(!is.null(attr)){
for(i in 1:NCOL(attr)){
  m<-match(as.character(attr$id),network.vertex.names(net))
  #net<-set.network.attribute(net, names(attr)[i], attr[m,i])
  net%v%names(attr)[i]<-attr[m,i]
}
#}

net
}


## Needle Sharing Network Flagstaff Rural
Flag_ego<-filter(ego,studynum=="Flagstaff Rural" & tietype == "Needle") #Needle
Flag_alter<-filter(alterAlter,studynum=="Flagstaff Rural" & tietype == "Needle")
head(Flag_ego)
dim(Flag_ego)
dim(Flag_alter)

Flag_alter<-filter(alterAlter,studynum=="Flagstaff Rural" & tietype == "Needle")
Flag_alter_sex<-filter(alterAlter,studynum=="Flagstaff Rural" & tietype == "Sexual")
Flag_alter_soc<-filter(alterAlter,studynum=="Flagstaff Rural" & tietype == "Social")
Flag_alter_drug<-filter(alterAlter,studynum=="Flagstaff Rural" & tietype == "Drug")


flag_needle_net<-buildSN(Flag_alter)
flag_sex_net<-buildSN(Flag_alter_sex)
flag_social_net<-buildSN(Flag_alter_soc)
flag_drug_net<-buildSN(Flag_alter_drug)

par(mfrow=c(2,2))
plot(flag_needle_net,vertex.col="race",main="Needle")
plot(flag_sex_net,vertex.col="race",main="Sex")
plot(flag_social_net,vertex.col="race",main="Social")
plot(flag_drug_net,vertex.col="race",main="Drug")



########### Social Network Ego is Homeless -- Flagstaff, AZ

Flag_alter_soc<-filter(alterAlter,studynum=="Flagstaff Rural" & tietype == "Social" & streets=="Yes")

flag_social_net<-buildSN(Flag_alter_soc)

plot(flag_social_net,vertex.col="race",main="Social")




########### Social Network Ego is Homeless -- Atlanta Urban
Atlanta_alter_soc<-filter(alterAlter,studynum=="Atlanta Urban" & tietype == "Social" & streets=="Yes")
Atlanta_social_net<-buildSN(Atlanta_alter_soc)


plot(Atlanta_social_net,vertex.col="race",main="Social")


