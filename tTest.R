## ----chunk_setup, include=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(echo = T, message=F, warning=F, comment=NA, autodep=F,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(width=120,digits=5,show.signif.stars=FALSE,scipen=5),fig.width=8, out.width="49%", fig.align = 'center', dev.args=list(bg = 'transparent'))


## ----echo=FALSE,include=FALSE-------------------------------------------------
mu0<-0
mu1<-13
delta<-mu1-mu0
sigmahat<-22


## -----------------------------------------------------------------------------
(qnorm(.975)+qnorm(.8))^2/((mu1-mu0)/sigmahat)^2


## ----power1-------------------------------------------------------------------
power.t.test(delta=mu1-mu0,sd=sigmahat,power=0.8,type="one.sample")


## -----------------------------------------------------------------------------
delta<-c(2,3,4,5,8)
alpha<-0.05
data.frame(delta=delta,n=qnorm(1-alpha/2)^2*(sigmahat/delta)^2)


## -----------------------------------------------------------------------------
set.seed(55)
mu<-10
sigma<-20
n<-40
Y<-rnorm(n,mu,sigma)


## -----------------------------------------------------------------------------
hist(Y)
summary(Y)


## -----------------------------------------------------------------------------
t.test(Y)


## -----------------------------------------------------------------------------
summary(mod<-lm(Y~1))


## -----------------------------------------------------------------------------
sqrt(sum(mod$residuals^2)/(n-1)) 


## -----------------------------------------------------------------------------
confint(mod)


## -----------------------------------------------------------------------------
wilcox.test(Y,conf.int=TRUE) 


## -----------------------------------------------------------------------------
n<-30 ##n per group
muInt<-21 ##True mean Int
muCont<-20 ##True mean Cont
Delta<-muInt-muCont ##True mean difference
Delta
sigma<-2 ##True standard deviation
set.seed(10)
dataInt <- rnorm(n=n,mean=muInt,sd=sigma) ##Sample from Int
dataCont <- rnorm(n=n,mean=muCont,sd=sigma) ##Sample from Cont
group<-gl(n=2,k=n,labels=c("Cont","Int"))##grouping variable
response<-c(dataCont,dataInt)##the outcome
mydata <- data.frame(response=response,group=group)##the data.frame
str(mydata)
head(mydata)


## -----------------------------------------------------------------------------
summary(mydata)
by(mydata,mydata$group,summary)
boxplot(response~group,mydata)


## -----------------------------------------------------------------------------
test <- t.test(x=dataInt,y=dataCont,var.equal=TRUE)
test


## -----------------------------------------------------------------------------
test$estimate ##Estimate of muInt and muCont
test$statistic ##t-statistic
test$stderr ##standard error 
((test$estimate[1]-test$estimate[2])-0)/test$stderr ##t-statistic "by hand"
test$p.value


## -----------------------------------------------------------------------------
test2 <- t.test(x=dataInt,y=dataCont,alternative="greater",var.equal=TRUE)
test2


## -----------------------------------------------------------------------------
test3 <- t.test(x=dataInt,y=dataCont,mu=-1,alternative="greater",var.equal=TRUE)
test3


## -----------------------------------------------------------------------------
test<- t.test(response~group,data=mydata,var.equal=TRUE)
test


## -----------------------------------------------------------------------------
mod<-lm(response~group,data=mydata)
summary(mod)
confint(mod)


## -----------------------------------------------------------------------------
mod2<-lm(response~1,data=mydata) ## Intercept-only model
anova(mod,mod2) ## tests the null: "Effect of group is absent"


## -----------------------------------------------------------------------------
plot(mod,which=c(1,2))


## -----------------------------------------------------------------------------
wilcox.test(dataInt,dataCont,conf.int=TRUE) 


## ----message=FALSE------------------------------------------------------------
library(TOSTER)
## Test observed mean of 0.52 and standard deviation of 0.5 in sample of 300 participants
## against 0.5 given equivalence bounds in raw units of -0.1 and 0.1, with an alpha = 0.05.
TOSTone.raw(m=0.52,mu=0.5,sd=0.5,n=300,low_eqbound=-0.1, high_eqbound=0.1, alpha=0.05)

