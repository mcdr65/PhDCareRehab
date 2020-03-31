## ----chunk_setup, include=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(echo = T, message=F, warning=F, comment=NA, autodep=F,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(width=120,options=4,show.signif.stars=FALSE),
                      fig.width=8, fig.align = 'center', dev.args=list(bg = 'transparent'))


## -----------------------------------------------------------------------------
data<-matrix(c(13,15,3,22,5,19),ncol=3,
             dimnames=list(Antwort=c("ja","nein"),Freundlichkeit=c("freundlich","forsch","sachlich")))
datatable<-as.table(data)
datatable
addmargins(datatable)
margin.table(datatable,1)
margin.table(datatable,2)
prop.table(datatable)
prop.table(datatable,1)
prop.table(datatable,2)
summary(datatable)


## -----------------------------------------------------------------------------
datadf<-as.data.frame(datatable)
datadf


## ----chisqtest----------------------------------------------------------------
chisq.test(datatable)


## ----fisher-------------------------------------------------------------------
fisher.test(datatable)


## ----As poisson-Modell--------------------------------------------------------
modglm<-glm(Freq~Antwort*Freundlichkeit,data=datadf,family="poisson")
modglm0<-glm(Freq~Antwort+Freundlichkeit,data=datadf,family="poisson")
anova(modglm,modglm0,test="Chisq")


## ----LogLinModel--------------------------------------------------------------
library(MASS)
loglm(~Antwort+Freundlichkeit,data=datatable)

