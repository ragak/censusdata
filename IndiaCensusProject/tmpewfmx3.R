setwd("C:/Users/Raga/Dropbox/Documents/Misc/IndiaCensusProject/")
data<-read.table("data.txt",sep="\t",header=TRUE)
sr01<-data[,4]
sr11<-data[,5]
gdp01<-data[,2]
gdp11<-data[,3]
#does sex ratio correlate over time
par( mfrow = c(2,2) )
plot(sr01,sr11,main="SR correlation",sub=cor.test(sr01,sr11,method="spearman")$estimate)
abline(lm(sr11~sr01), col="red")
cor.test(sr01,sr11,method="spearman")
#does gdp correlate over time
plot(gdp01,gdp11,main="GDP correlation",sub=cor.test(gdp01,gdp11,method="spearman")$estimate)
title("GDP correlation")
abline(lm(gdp11~gdp01), col="red")
cor.test(gdp01,gdp11,method="spearman")
#first observation - sex ratio is less correlated between 2001 and 2011 than gdp
#what about sex ratio vs GDP?
plot(sr01,gdp01,main="SRvsGDP 2001 correlation",sub=cor.test(sr01,gdp01,method="spearman")$estimate)
abline(lm(sr01~gdp01), col="red")
cor.test(gdp01,gdp11,method="spearman")

plot(sr11,gdp11,main="SRvsGDP 2011 correlation",sub=cor.test(sr11,gdp11,method="spearman")$estimate)
abline(lm(sr11~gdp11), col="red")
cor.test(gdp01,gdp11,method="spearman")

#can we model the relationship?
#variales are GDP and sex ratio
#for all states we have GDP and sr from 2001 and 2011
years<-c(rep("yr2001",32),rep("yr2011",32))
#type<-c(rep("sr",64),rep("gdp",64))
sr<-c(sr01,sr11)
gdp<-c(gdp01,gdp11)
final<-cbind(sr,gdp,years)
final<-data.frame(final)

#linear model
# Multiple Linear Regression Example 
#first build model matrix
library(rafalib)
X<-model.matrix(sr ~ gdp + years, data=final)
colnames(X)
head(X)
imagemat(X,main="model matrix for sex ratio with GDP and year as variables")
fit <- lm(sr ~ gdp + years, data=final)
summary(fit) # show results# Multiple Linear Regression Example 

