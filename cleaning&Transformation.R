#DATA CLEANING AND TRANSFORMATION IN R USING SLEEP DATASET WHICH IS IN-BUILT IN PACKAGE CALLED VIM
# data() COMMAND USED TO SEE IN-BUILT DATASET IN R 

library(VIM)
library(naniar)
library(ggplot2)
library(DataExplorer)
data(sleep, package = "VIM")
sp=sleep
plot_missing(sp)
qplot(sp$BodyWgt,sp$Sleep)
plot_histogram(sp)
boxplot(sp$Sleep)
gg_miss_var(sp)
res<-summary(aggr(sp, sortVar=TRUE))$combinations
head(sp)
tail(sp)
mean(sp$NonD,na.rm=TRUE)
mean(sp$Sleep,na.rm=TRUE)
max(sp$Span,na.rm=TRUE)
summary(sp)
sp$NonD=ifelse(is.na(sp$NonD),median(sp$NonD,na.rm = TRUE),sp$NonD)
sp$Dream=ifelse(is.na(sp$Dream),median(sp$Dream,na.rm = TRUE),sp$Dream)
sp$Sleep=ifelse(is.na(sp$Sleep),median(sp$Sleep,na.rm = TRUE),sp$Sleep)
sp$Span=ifelse(is.na(sp$Span),median(sp$Span,na.rm = TRUE),sp$Span)
sp$Gest=ifelse(is.na(sp$Gest),median(sp$Gest,na.rm = TRUE),sp$Gest)
summary(sp)
head(sp)
sp$Harmful.WgtGain=sp$BodyWgt>100
head(sp)
intervals=c(0,5,10,15,20,25,30,35)
intervals
sp$Dream=cut(sp$Dream,breaks= intervals,include.lowest = TRUE)
head(sp)
span_set=c(0,20,40,60,80,100,120,140)
span_set
sp$Span=cut(sp$Span,breaks= span_set,include.lowest = TRUE)
head(sp)
s1=sp
s1$Danger=gsub(1,'Very low',s1$Danger)
s1$Danger=gsub(2,'Low',s1$Danger)
s1$Danger=gsub(3,'Medium',s1$Danger)
s1$Danger=gsub(4,'High',s1$Danger)
s1$Danger=gsub(5,'Very High',s1$Danger)
tail(s1)
head(s1)
gg_miss_var(sp)
res<-summary(aggr(sp, sortVar=TRUE))$combinations
# Using transform function 
sp_ex1 <- transform(sp, Pred = Pred + 1)   
head(sp_ex1) 
