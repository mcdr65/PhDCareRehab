## ----chunk_setup, include=FALSE, eval=TRUE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = T, message=F, warning=F, comment=NA, autodep=F,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(width=120,digits=5,show.signif.stars=FALSE),
                      fig.width=8, fig.align = 'center', dev.args=list(bg = 'transparent'))


## ------------------------------------------------------------------------------------------------------------------------------
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


## ----chisqtest-----------------------------------------------------------------------------------------------------------------
chisq.test(datatable)


## ----fisher--------------------------------------------------------------------------------------------------------------------
fisher.test(datatable)


## ------------------------------------------------------------------------------------------------------------------------------
datadf<-as.data.frame(datatable)
datadf


## ----As poisson-Modell---------------------------------------------------------------------------------------------------------
modglm<-glm(Freq~Antwort*Freundlichkeit,data=datadf,family="poisson")
modglm0<-glm(Freq~Antwort+Freundlichkeit,data=datadf,family="poisson")
anova(modglm,modglm0,test="LRT")


## ----summarPois----------------------------------------------------------------------------------------------------------------
summary(modglm)


## ----Alternative Parametriesierng----------------------------------------------------------------------------------------------
summary(modglmMeans<-glm(Freq~Antwort:Freundlichkeit-1,data=datadf,family="poisson"))



## ------------------------------------------------------------------------------------------------------------------------------
as.data.frame(exp(summary(modglmMeans)$coef[,1]))


## ------------------------------------------------------------------------------------------------------------------------------
confint(modglmMeans)


## ----LogLinModel---------------------------------------------------------------------------------------------------------------
library(MASS)
loglm(~Antwort+Freundlichkeit,data=datatable)

