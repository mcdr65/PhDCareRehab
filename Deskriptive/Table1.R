### R code from vignette source '/home/meichtry/BERATUNG/PhysioFE/OMEGA/RCTJuli14/Table1.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: Table1.Rnw:42-63
###################################################
rm(list=ls())
setwd("/home/meichtry/BERATUNG/PhysioFE/OMEGA/RCTJuli14")
dir()
load("Juli14.Rdata")
ls()
library(MASS)
library(languageR)
library(nlme)
library(psych)
library(lme4)
library(lattice)
library(sfsmisc)
library(lsmeans)
library(doBy)
library(multcomp)
#library(lmerTest)
library(multcompView)
library(pbkrtest)
options(width=120,digits=4,scipen=2)
data.frame(names(d.wide))
str(d.wide)


###################################################
### code chunk number 2: Table1.Rnw:74-81
###################################################
BL<- seq(13,187,by=5)
fact <- c(1,2,3,5,8,11)
cont <- c(4,6,7,9,10,12)
tmp<-describeBy(d.wide[,cont],group=d.wide$group,skew=FALSE)
tmp2 <- data.frame(Ctrl.Mean=tmp$control$mean,Ctrl.SD=tmp$control$sd,Int.Mean=tmp$intervention$mean,Int.SD=tmp$intervention$sd)
rownames(tmp2) <- names(d.wide)[cont]
tmp2


###################################################
### code chunk number 3: Table1.Rnw:91-98
###################################################
BL<- seq(13,187,by=5)
fact <- c(1,2,3,5,8,11)
cont <- c(4,6,7,9,10)
tmp<-describeBy(d.wide[,BL],group=d.wide$group,skew=FALSE)
tmp2 <- data.frame(Ctrl.Mean=tmp$control$mean,Ctrl.SD=tmp$control$sd,Int.Mean=tmp$intervention$mean,Int.SD=tmp$intervention$sd)
rownames(tmp2) <- names(d.wide)[BL]
tmp2


###################################################
### code chunk number 4: Table1.Rnw:106-108
###################################################
mw.grouped <- split(d.wide[,fact[-3]],f=d.wide$group)
lapply(mw.grouped,summary)


