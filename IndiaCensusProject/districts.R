rm(list=ls())
setwd("C:/Users/Raga/Documents/GitHub/censusdata/IndiaCensusProject/")
data1<-read.table("SRdist19912001.txt",sep="\t",header=TRUE)
data2<-read.table("SRdist2011.txt",sep="\t",header=TRUE)
data3<-merge(data1,data2,by= "District", all=TRUE)
data4<-na.omit(data3)
district<-data4[,1]
state<-data4[,2]
sr91<-data4[,6]
sr01<-data4[,7]
sr11<-data4[,11]

#compare SR across districts - does it correlate over time?
par( mfrow = c(1,3) )

plot(sr11,sr91,main="SRdist 1991 vs 2011 correlation",sub=cor.test(sr11,sr91,method="spearman")$estimate)
abline(lm(sr91~sr11), col="red")

plot(sr11,sr01,main="SRdist 1991 vs 2001 correlation",sub=cor.test(sr11,sr01,method="spearman")$estimate)
abline(lm(sr01~sr11), col="red")

plot(sr01,sr11,main="SRdist 2001 vs 2011 correlation",sub=cor.test(sr01,sr11,method="spearman")$estimate)
abline(lm(sr11~sr01), col="red")
#what about with literacy?
lit11<-data4[,12]
par( mfrow = c(1,1) )
plot(lit11,sr11,main="SRdist vs litdist 2011 correlation",sub=cor.test(lit11,sr11,method="spearman")$estimate)
abline(lm(sr11~lit11), col="red")
#no correlation - may need to subdivide based on extent of change
#look at growth rate
gro91<-data4[,4]
gro01<-data4[,5]
gro11<-data4[,10]
gro11<-gsub("%","",gro11)
gro11<-as.numeric(gro11)
#compare growth rate across districts - does it correlate over time?
par( mfrow = c(1,3) )

plot(gro11,gro91,main="Gro-dist 1991 vs 2011 correlation",sub=cor.test(gro11,gro91,method="spearman")$estimate)
abline(lm(gro91~gro11), col="red")

plot(gro01,gro91,main="Gro-dist 1991 vs 2001 correlation",sub=cor.test(gro01,gro91,method="spearman")$estimate)
abline(lm(gro91~gro01), col="red")

plot(gro01,gro11,main="Gro-dist 2001 vs 2011 correlation",sub=cor.test(gro01,gro11,method="spearman")$estimate)
abline(lm(gro11~gro01), col="red")

#24 out of 531 districts for which all data is available had NO growth in population in 11
#could that correlate with SR?
nogro<-(sr11[gro11==0])
nogrosd<-sd(sr11[gro11==0])
gro<-(sr11[gro11!=0])
gro<-sd(sr11[gro11!=0])
#is growth normally distributed?
par( mfrow = c(1,2) )
qqnorm(gro01)
hist(gro01)
qqnorm(gro91)
hist(gro91)
qqnorm(gro11)
hist(gro11)

#not normal, us K-S test to see if they are different (may be try wilcox too?)
ks.test(gro,nogro)
#need a bootstrop