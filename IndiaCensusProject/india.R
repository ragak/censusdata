rm(list=ls())
setwd("C:/Users/Raga/Dropbox/India census/IndiaCensusProject/")
data<-read.table("data.txt",sep="\t",header=TRUE)
sr01<-data[,9]
sr11<-data[,11]
asr01<-data[,8]
asr11<-data[,10]
gdp01<-data[,4]
gdp11<-data[,5]
gdp01gross<-data[,2]
gdp11gross<-data[,3]
pop01<-data[,6]
pop11<-data[,7]
gr11<-data[,12]
area11<-data[,13]
den11<-data[,14]
lit11<-data[,15]
lit01<-data[,16]
rural01<-data[,17]
urban01<-data[,18]
tot01<-data[,19]
rural11<-data[,20]
urban11<-data[,21]
tot11<-data[,22]
sr91<-data[,23]
sr91to6<-data[,25]
sr01to6<-data[,26]
sr91ab7<-data[,27]
sr01ab7<-data[,28]
sr11to6<-data[,33]
lit81<-data[,30]
lit91<-data[,31]

#does sex ratio correlate over time
par( mfrow = c(2,2) )
plot(sr01,sr11,main="SR correlation",sub=cor.test(sr01,sr11,method="spearman")$estimate)
abline(lm(sr11~sr01), col="red")
cor.test(sr01,sr11,method="spearman")

#does gdp correlate over time
plot(gdp01,gdp11,main="GDP per capita correlation",sub=cor.test(gdp01,gdp11,method="spearman")$estimate)
abline(lm(gdp11~gdp01), col="red")
cor.test(gdp01,gdp11,method="spearman")

#gdp and population individually
plot(gdp01gross,gdp11gross,main="gross GDP correlation",sub=cor.test(gdp01gross,gdp11gross,method="spearman")$estimate)
abline(lm(gdp11gross~gdp01gross), col="red")
cor.test(gdp01gross,gdp11gross,method="spearman")
plot(pop01,pop11,main="population correlation",sub=cor.test(pop01,pop11,method="spearman")$estimate)
abline(lm(pop11~pop01), col="red")
cor.test(pop01,pop11,method="spearman")

#what about sex ratio vs GDP?
par( mfrow = c(1,2) )

plot(sr01,gdp01,main="SRvsGDP 2001 correlation",sub=cor.test(sr01,gdp01,method="spearman")$estimate)
abline(lm(sr01~gdp01), col="red")
cor.test(sr01,gdp01,method="spearman")

plot(sr11,gdp11,main="SRvsGDP 2011 correlation",sub=cor.test(sr11,gdp11,method="spearman")$estimate)
abline(lm(sr11~gdp11), col="red")
cor.test(sr11,gdp11,method="spearman")

#spearman's is a rank correlation - so let's visualize by rank
gdprank01<-rank(gdp01)
gdprank11<-rank(gdp11)
srrank01<-rank(sr01)
srrank11<-rank(sr11)
rankdata<-cbind(data[,1],gdprank01,gdprank11,srrank01,srrank11)

par( mfrow = c(1,2) )

plot(srrank01,gdprank01,main="SRvsGDP 2001 rank correlation",sub=cor.test(srrank01,gdprank01,method="spearman")$estimate)
abline(lm(srrank01~gdprank01), col="red")
cor.test(srrank01,gdprank01,method="spearman")

plot(srrank11,gdprank11,main="SRvsGDP 2011 rank correlation",sub=cor.test(srrank11,gdprank11,method="spearman")$estimate)
abline(lm(srrank11~gdprank11), col="red")
cor.test(srrank11,gdprank11,method="spearman")

#surprisingly they seem unrelated

#can we model the relationship?
#variales are GDP and sex ratio
#for all states we have GDP and sr from 2001 and 2011
years<-c(rep("yr2001",32),rep("yr2011",32))
#type<-c(rep("sr",64),rep("gdp",64))
sr<-c(sr01,sr11)
gdp<-c(gdp01,gdp11)
final<-cbind(sr,gdp,years)
final<-data.frame(final)

#linear model is not the best for this relationship, and even though there seems to be
#no visible relationship can we try and see if there is one?
#use maximal information coefficient on the data 
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/
library(minerva)
mine(as.numeric(final$sr), as.numeric(final$gdp), master=final$yr)
#MIC ranges from 0-1, where 0 is independent and 1 is perfectly related
#here it's 0.28, so more unrelated than related, and all the other stats suggest
#it is not really captured by a function, confirming lack of correlation 

#what about adult ratios?
plot(asr11,asr01,main="Adult ratios 2001 vs 2011 correlation",sub=cor.test(asr11,asr01,method="spearman")$estimate)
abline(lm(asr11~asr01), col="red")
cor.test(asr11,asr01,method="spearman")

#what about comparing adult and child ratios?
plot(asr11,sr11,main="Adult vs child ratios 2011 correlation",sub=cor.test(asr11,sr11,method="spearman")$estimate)
abline(lm(sr11~asr11), col="red")
cor.test(asr11,sr11,method="spearman")

plot(asr01,sr01,main="Adult vs child ratios 2001 correlation",sub=cor.test(asr01,sr01,method="spearman")$estimate)
abline(lm(sr01~asr01), col="red")
cor.test(asr01,sr01,method="spearman")

#what about comparing more metrics just in 2011?
plot(sr11,area11,main="Ratio vs area 2011 correlation",sub=cor.test(sr11,area11,method="spearman")$estimate)
abline(lm(area11~sr11), col="red")
cor.test(sr11,area11,method="spearman")

plot(sr11,area11,main="Ratio vs area 2011 correlation",sub=cor.test(sr11,area11,method="spearman")$estimate)
abline(lm(area11~sr11), col="red")
cor.test(sr11,area11,method="spearman")

plot(sr11,gr11,main="Ratio vs growth 2011 correlation",sub=cor.test(sr11,gr11,method="spearman")$estimate)
abline(lm(gr11~sr11), col="red")
cor.test(sr11,gr11,method="spearman")

plot(sr11,den11,main="Ratio vs density 2011 correlation",sub=cor.test(sr11,den11,method="spearman")$estimate)
abline(lm(den11~sr11), col="red")
cor.test(sr11,den11,method="spearman")

plot(sr11,lit11,main="Ratio vs literacy 2011 correlation",sub=cor.test(sr11,lit11,method="spearman")$estimate)
abline(lm(lit11~sr11), col="red")
cor.test(sr11,lit11,method="spearman")

#but is gdp related to literacy?

plot(gdp11,lit11,main="GDP vs literacy 2011 correlation",sub=cor.test(gdp11,lit11,method="spearman")$estimate)
abline(lm(lit11~gdp11), col="red")
cor.test(gdp11,lit11,method="spearman")

#much more than sex ratio is (rho = 0.72)

#try pearson correlation (non-rank based)

plot(sr11,lit11,main="Ratio vs literacy 2011 pearson",sub=cor.test(sr11,lit11,method="pearson")$estimate)
abline(lm(lit11~sr11), col="red")
cor.test(sr11,lit11,method="pearson")

#but is gdp related to literacy?

plot(gdp11,lit11,main="GDP vs literacy 2011 pearson",sub=cor.test(gdp11,lit11,method="pearson")$estimate)
abline(lm(lit11~gdp11), col="red")
cor.test(gdp11,lit11,method="pearson")

#try rank for sex ratio, literacy and GDP
#gdprank01<-rank(gdp01)
#gdprank11<-rank(gdp11)
#srrank01<-rank(sr01)
#srrank11<-rank(sr11)
litrank11<-rank(lit11)

plot(srrank11,litrank11,main="Ratio vs literacy rank 2011 correlation",sub=cor.test(srrank11,litrank11,method="spearman")$estimate)
abline(lm(litrank11~srrank11), col="red")
cor.test(srrank11,litrank11,method="spearman")

plot(gdprank11,litrank11,main="GDP vs literacy rank 2011 correlation",sub=cor.test(gdprank11,litrank11,method="spearman")$estimate)
abline(lm(litrank11~gdprank11), col="red")
cor.test(gdprank11,litrank11,method="spearman")


#even though the literature says sex ratio is related to literacy, it doesn't seem like it

final<-cbind(sr11,lit11,gdp11)
final<-data.frame(final)

#linear model is not the best for this relationship, and even though there seems to be
#no visible relationship can we try and see if there is one?
#use maximal information coefficient on the data 
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/
library(minerva)
SRvLITmine<-mine(as.numeric(final$sr11), as.numeric(final$lit11))
SRvGDP11<-mine(as.numeric(final$sr11), as.numeric(final$gdp11))
GDPvLIT11<-mine(as.numeric(final$gdp11), as.numeric(final$lit11))

#literacy over time vs SR over time

plot(lit11,lit01,main="Literacy 2001 v 2011 correlation",sub=cor.test(lit11,lit01,method="spearman")$estimate)
abline(lm(lit01~lit11), col="red")
plot(sr11,lit01,main="Literacy v SR 2001 correlation",sub=cor.test(sr01,lit01,method="spearman")$estimate)
abline(lm(lit01~sr01), col="red")
#change in literacty vs change in SR
deltaLit<-lit11-lit01
deltaSR<-sr11-sr01
plot(deltaSR,deltaLit,main="Delta Literacy v SR correlation",sub=cor.test(deltaSR,deltaLit,method="spearman")$estimate)
abline(lm(deltaLit~deltaSR), col="red")

#no relation! what about GDP and literacy change?
deltaGDP<-gdp11-gdp01
plot(deltaGDP,deltaLit,main="Delta Literacy v GDP correlation",sub=cor.test(deltaGDP,deltaLit,method="spearman")$estimate)
abline(lm(deltaLit~deltaGDP), col="red")

#not much relation (if anything it's negative). let's try minimum info

years<-c(rep("yr2001",32),rep("yr2011",32))
#type<-c(rep("sr",64),rep("gdp",64))
sr<-c(sr01,sr11)
gdp<-c(gdp01,gdp11)
lit<-c(lit01,lit11)
final<-cbind(sr,gdp,lit,years)
final<-data.frame(final)

#linear model is not the best for this relationship, and even though there seems to be
#no visible relationship can we try and see if there is one?
#use maximal information coefficient on the data 
#http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3325791/
library(minerva)
mine(as.numeric(final$sr), as.numeric(final$gdp), master=final$yr)
mine(as.numeric(final$sr), as.numeric(final$lit), master=final$yr)
mine(as.numeric(final$gdp), as.numeric(final$lit), master=final$yr)

#let's look at urban vs rural
data2<-na.omit(data)
sr01<-data2[,9]
sr11<-data2[,11]
rural01<-data2[,17]
urban01<-data2[,18]
tot01<-data2[,19]
rural11<-data2[,20]
urban11<-data2[,21]
tot11<-data2[,22]
gdp01<-data2[,4]
gdp11<-data2[,5]
popratio01<-rural01/tot01
popratio11<-rural11/tot11

plot(popratio01,popratio11,main="Rural ratio 01 v 11 correlation",sub=cor.test(popratio01,popratio11,method="spearman")$estimate)
abline(lm(popratio11~popratio01), col="red")

deltapopratio<-popratio11-popratio01
deltasr<-sr11-sr01
deltagdp<-gdp11-gdp01

plot(deltapopratio,deltasr,main="Delta Rural ratio vs SR correlation",sub=cor.test(deltapopratio,deltasr,method="spearman")$estimate)
abline(lm(deltasr~deltapopratio), col="red")

plot(deltapopratio,deltagdp,main="Delta Rural ratio vs GDP correlation",sub=cor.test(deltapopratio,deltagdp,method="spearman")$estimate)
abline(lm(deltagdp~deltapopratio), col="red")

years<-c(rep("yr2001",29),rep("yr2011",29))
sr<-c(sr01,sr11)
gdp<-c(gdp01,gdp11)
popr<-c(popratio01,popratio11)
final<-cbind(sr,gdp,popr,years)
final<-data.frame(final)
library(minerva)
mine(as.numeric(final$sr), as.numeric(final$popr), master=final$yr)
mine(as.numeric(final$gdp), as.numeric(final$popr), master=final$yr)

#got 91 literacy and SR data
data<-na.omit(data)
sr01<-data[,9]
sr11<-data[,11]
asr01<-data[,8]
asr11<-data[,10]
gdp01<-data[,4]
gdp11<-data[,5]
gdp01gross<-data[,2]
gdp11gross<-data[,3]
pop01<-data[,6]
pop11<-data[,7]
gr11<-data[,12]
area11<-data[,13]
den11<-data[,14]
lit11<-data[,15]
lit01<-data[,16]
rural01<-data[,17]
urban01<-data[,18]
tot01<-data[,19]
rural11<-data[,20]
urban11<-data[,21]
tot11<-data[,22]
sr91<-data[,23]
sr91to6<-data[,25]
sr01to6<-data[,26]
sr91ab7<-data[,27]
sr01ab7<-data[,28]
sr11to6<-data[,33]
lit81<-data[,30]
lit91<-data[,31]
#compare over time 
par( mfrow = c(1,3) )
plot(lit91,lit01,main="Literacy 91 v 01 correlation",sub=cor.test(lit91,lit01,method="spearman")$estimate)
abline(lm(lit01~lit91), col="red")
plot(lit91,lit11,main="Literacy 91 v 11 correlation",sub=cor.test(lit91,lit11,method="spearman")$estimate)
abline(lm(lit11~lit91), col="red")
plot(lit01,lit11,main="Literacy 01 v 11 correlation",sub=cor.test(lit01,lit11,method="spearman")$estimate)
abline(lm(lit11~lit01), col="red")

#compare to SR
sr91<-as.numeric(sr91)
plot(lit91,sr91,main="Literacy v SR 91 correlation",sub=cor.test(lit91,sr91,method="spearman")$estimate)
abline(lm(sr91~lit91), col="red")
plot(lit01,sr01,main="Literacy v SR 01 correlation",sub=cor.test(lit01,sr01,method="spearman")$estimate)
abline(lm(sr01~lit01), col="red")
plot(lit11,sr11,main="Literacy v SR 11 correlation",sub=cor.test(lit11,sr11,method="spearman")$estimate)
abline(lm(sr11~lit11), col="red")

years<-c(rep("yr1991",24),rep("yr2001",24),rep("yr2011",24))
sr<-c(sr91,sr01,sr11)
lit<-c(lit91,lit01,lit11)
final<-cbind(sr,lit,years)
final<-data.frame(final)
library(minerva)
mine(as.numeric(final$sr), as.numeric(final$lit), master=final$years)

#number of over million population agglomerations in 2001 and 2011
states<-as.character(data[,1])
urbanagg<-read.table("urbanagg.txt",sep="\t",header=TRUE)
search<-function(x){
  sum(urbanagg$state==states[x])}
n<-length(states)
urb11<-sapply(1:n,search)
#for 2001
urbanagg<-urbanagg[urbanagg[,4]>1000000,]
urb01<-sapply(1:n,search)
urbanaggall<-cbind(states,urb2001,urb2011)
colnames(urbanaggall) <- c("states","urb2001","urb2011")
urbanaggall<-data.frame(urbanaggall)

plot(urb01,urb11,main="Urban agglomerations 01 v 11 correlation",
     sub=cor.test(urb01,urb11,method="spearman")$estimate)
abline(lm(urb11~urb01), col="red")

plot(urb01,sr01,main="Urban agglomerations v SR 01 correlation",
     sub=cor.test(urb01,sr01,method="spearman")$estimate)
abline(lm(sr01~urb01), col="red")

plot(urb11,sr11,main="Urban agglomerations v SR 11 correlation",
     sub=cor.test(urb11,sr11,method="spearman")$estimate)
abline(lm(sr11~urb11), col="red")

plot(urb11,lit11,main="Urban agglomerations v Lit 11 correlation",
     sub=cor.test(urb11,lit11,method="spearman")$estimate)
abline(lm(lit11~urb11), col="red")

plot(urb11,gdp11,main="Urban agglomerations v GDP 11 correlation",
     sub=cor.test(urb11,gdp11,method="spearman")$estimate)
abline(lm(gdp11~urb11), col="red")

#urban agglomerations don't seem to correlate with much. try delta urban agg
deltaurb<-urb11-urb01
deltasr<-sr11-sr01
deltalit<-lit11-lit01
deltagdp<-gdp11-gdp01

par( mfrow = c(1,1) )

plot(deltaurb,deltasr,main="Delta urban agglomerations v delta SR 01 to 11 correlation",
     sub=cor.test(deltaurb,deltasr,method="spearman")$estimate)
abline(lm(deltasr~deltaurb), col="red")

#a study http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3000011/ says that unless a certain
#threshold of education is passed, the increase in education may actually be causing
#an increase in female mortality, since not enough people are educated to shift the 
#general mass opinion that females and males are equal, but the educated people have
#more access to prenatal screening and can therefore terminate female pregnancies
#very interesting thought.. can we identify such a threshold?

#linear model
# Multiple Linear Regression Example 
#first build model matrix
#library(rafalib)
#X<-model.matrix(sr ~ gdp + years, data=final)
#colnames(X)
#head(X)
#imagemat(X,main="model matrix for sex ratio with GDP and year as variables")
#fit <- lm(sr ~ gdp + years, data=final)
#summary(fit) # show results# Multiple Linear Regression Example 
