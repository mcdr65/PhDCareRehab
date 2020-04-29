## ----chunk_setup, include=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(echo = T, out.width="49%",message=F, warning=F, comment=NA,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(width=200,digits=3,show.signif.stars=FALSE),dev.args=list(bg = 'transparent'))


## ----needed,echo=TRUE,results="hide"------------------------------------------
library(lme4)
library(lmerTest)
library(psych)
library(ggplot2)


## -----------------------------------------------------------------------------
RepData<-function(n=30,I=2,mu=20,alpha=runif(I-1,mu/2,mu),nu=10,tau=5)
{
    N<-n*I ##observations
    set.seed(4)
    subject<-gl(n,I,N,labels=paste("s",1:n,sep=""))
    U<-rep(rnorm(n,0,nu),each=I)##random intercept
    E <- rnorm(N,0,tau)##random error
    time <- gl(I,1,N,labels=paste("t",1:I,sep=""))
    X<-model.matrix(~time)
    fixed <- c(mu,alpha) ##parameters (mu, alpha)
    response <- X%*%fixed+U+E ##systematic part + random intercept + random error
    data <- data.frame(subject,time,response)
    parameters<-c(n=n,I=I,mu=mu,alpha=alpha,nu=nu,tau=tau,rho=nu^2/(nu^2+tau^2))
    l<-list(data=data,parameters=parameters)
    return(l)
}    


## -----------------------------------------------------------------------------
tmp<-RepData() ##with default arguments
d.long2<-tmp$data
parms<-tmp$parameters


## -----------------------------------------------------------------------------
parms


## -----------------------------------------------------------------------------
aggregate(response~time,data=d.long2,summary)
with(d.long2,interaction.plot(time,subject,response))


## -----------------------------------------------------------------------------
p <- ggplot(data = d.long2, aes(x = time, y = response, group = subject))
p <- p+geom_point()+geom_line()+stat_smooth(aes(group = 1),method="lm",se=FALSE)
p <- p + stat_summary(aes(group=1), geom = "point", fun.y = mean,shape = 17, size = 4)
p


## -----------------------------------------------------------------------------
t.test(response~time,paired=TRUE,data=d.long2)
cor(d.long2$response[d.long2$time=="t1"],d.long2$response[d.long2$time=="t2"])


## -----------------------------------------------------------------------------
x<-d.long2$response[d.long2$time=="t1"]
y<-d.long2$response[d.long2$time=="t2"]
t.test(y-x)


## -----------------------------------------------------------------------------
cor(x,y)


## -----------------------------------------------------------------------------
modelRep1<-aov(response~time+Error(subject),data=d.long2)
print(summary(modelRep1),digits=4)


## -----------------------------------------------------------------------------
mod0 <- lm(response~1,d.long2) 
mods <- lm(response~subject,d.long2)
modt <- lm(response~time,d.long2)
modts <-lm(response~subject+time,d.long2)


## -----------------------------------------------------------------------------
rss.0 <- sum((mod0$residuals)^2)
#(ss.0<-sum((d.long2$response-mod0$fitted)^2)) ##equivalent...
rss.s <- sum((mods$residuals)^2)
rss.t <- sum((modt$residuals)^2)
rss.ts<- sum((modts$residuals)^2)


## -----------------------------------------------------------------------------
rss.0
rss.0-rss.s
rss.0-rss.t
rss.0-rss.ts
rss.ts


## -----------------------------------------------------------------------------
lmm1<-lmer(response~time+(1|subject), data=d.long2)
summary(lmm1,cor=FALSE)


## -----------------------------------------------------------------------------
parms


## -----------------------------------------------------------------------------
anova(lmm1)
plot(lmm1)


## -----------------------------------------------------------------------------
tmp<-RepData(n=50,I=4,mu=20,alpha=c(2,3,4),nu=10,tau=5)
d.long<-tmp$data
parms<-tmp$parameters


## -----------------------------------------------------------------------------
parms


## ----fig.show="hold"----------------------------------------------------------
aggregate(response~time,data=d.long,summary)
with(d.long,interaction.plot(time,subject,response))
p <- ggplot(data = d.long, aes(x = time, y = response, group = subject))
p <- p+geom_point()+geom_line()+stat_smooth(aes(group = 1),method="lm",se=FALSE)
p <- p + stat_summary(aes(group=1), geom = "point", fun.y = mean,shape = 17, size = 4)
p


## -----------------------------------------------------------------------------
modelRep2 <-aov(response~time+Error(subject),data=d.long)
summary(modelRep2)


## -----------------------------------------------------------------------------
lmm2 <- lmer(response~time+(1|subject),data=d.long)
summary(lmm2,cor=FALSE)
anova(lmm2)


## -----------------------------------------------------------------------------
plot(lmm2)


## -----------------------------------------------------------------------------
RepData2<-function(n=100,mu=100,alpha=3,beta=5,gamma=0,nu=10,tau=5)
{
    N<-n*2 ##observations
    set.seed(65)
    subject<-gl(n,2,N,labels=paste("s",1:n,sep=""))
    U<-rep(rnorm(n,0,nu),each=2)##random intercept
    E <- rnorm(N,0,tau)##random error
    time <- gl(2,1,N,labels=paste("t",1:2,sep=""))
    group <- as.factor(c(rep("Ctr",n),rep("Trt",n)))
    X<-model.matrix(~time*group)
    fixed <- c(mu,alpha,beta,gamma) ##parameters (mu, alpha, beta, gamma)
    response <- X%*%fixed+U+E ##systematic part + random intercept + random error
    data <- data.frame(subject,time,group,response)
    parameters<-c(n=n,mu=mu,alpha=alpha,beta=beta,gamma=gamma,nu=nu,tau=tau,rho=nu^2/(nu^2+tau^2))
    l<-list(data=data,parameters=parameters)
    return(l)
}

tmp<-RepData2() ## with default arguments
d.longB<-tmp$data
parms<-tmp$parameters


## -----------------------------------------------------------------------------
headTail(d.longB)


## ----fig.show="hold"----------------------------------------------------------
with(d.longB,interaction.plot(time,group,response))
## with(d.longB,interaction.plot(time,subject,response))
p <- ggplot(data = d.longB, aes(x = time, y = response, group = subject))
p <- p + geom_line() + facet_grid(. ~ group)
p <- p + stat_smooth(aes(group = 1), method = "lm", se = FALSE) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3)
p


## -----------------------------------------------------------------------------
aggregate(response~time+group,data=d.longB,summary)


## -----------------------------------------------------------------------------
modelRep3 <-aov(response~time*group+Error(subject/time),data=d.longB) ##+Error(subject) is equivalent
print(summary(modelRep3),digits=4)


## -----------------------------------------------------------------------------
lmm3 <- lmer(response~time*group+(1|subject),data=d.longB)
summary(lmm3,cor=FALSE)
anova(lmm3)


## -----------------------------------------------------------------------------
parms


## -----------------------------------------------------------------------------
predicted<-predict(lmm3)
p <- ggplot(data = d.longB, aes(x = time, y = predicted, group = subject))
p <- p + geom_line() + facet_grid(. ~ group)
p <- p + stat_smooth(aes(group = 1), method = "lm", se = FALSE) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 17, size = 3)
p


## -----------------------------------------------------------------------------
plot(lmm3)

