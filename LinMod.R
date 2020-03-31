## ----chunk_setup, include=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(echo = T, message=F, warning=F, comment=NA, autodep=F,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(width=120,digits=2,show.signif.stars=FALSE,scipen=0),
                      out.width="49%", dev.args=list(bg = 'transparent'))


## -----------------------------------------------------------------------------
library(psych)


## -----------------------------------------------------------------------------
set.seed(10)  
n.groups <- 2
n.sample <- 30
n <- n.groups*n.sample ##sample size
ind <- rep(1:n.groups, each=n.sample) ##Indicator for group
group <- factor(ind, labels = c("A", "B"))
height <- rnorm(n, mean=165, sd=11.4)
covariates<-data.frame(group,height)
Xeffects <- model.matrix(~group*height)
Xmeans <- model.matrix(~group*height-height-1)
sigma <-2
betaMeans <- c(muA<--36.475,muB<--45.5,slopeA<-0.615,slopeB<-0.7)
betaEffects <- c(muA,muB-muA,slopeA,slopeB-slopeA)
lin.pred <- Xeffects %*% betaEffects 	
lin.pred2 <- Xmeans %*% betaMeans  
#all.equal(lin.pred,lin.pred2) ## should be same of course
eps <- rnorm(n = n, mean = 0, sd = sigma) ## add noise
weight <- lin.pred + eps ## response
df <- data.frame(group,height,weight)


## -----------------------------------------------------------------------------
str(df)
headTail(df)
plot(weight~height,data=df,col=as.numeric(group))
legend(140,80,legend=levels(group),col=c(1,2),pch=21)


## -----------------------------------------------------------------------------
by(df[,-1],df$group,describe) 
cor(weight,height,method="pearson")
cor(weight,height,method="spearman")
by(df[,-1],df$group,cor)


## -----------------------------------------------------------------------------
mod<-lm(weight~group*height,df)
mod


## -----------------------------------------------------------------------------
summary(mod)


## -----------------------------------------------------------------------------
betaEffects
sigma


## -----------------------------------------------------------------------------
plot(weight~height,data=df,col=as.numeric(group))
legend(150,80,legend=levels(group),col=c(1,2),pch=21)
abline(mod$coef[1],mod$coef[3],col=1)
abline(mod$coef[1]+mod$coef[2],mod$coef[3]+mod$coef[4],col=2)


## -----------------------------------------------------------------------------
confint(mod,level=0.95)


## -----------------------------------------------------------------------------
knitr::kable(cbind(summary(mod)$coef,confint(mod)),digits=3)


## -----------------------------------------------------------------------------
knitr::kable(anova(mod),digits=3)


## -----------------------------------------------------------------------------
modNoMainG<-lm(weight~height*group-group) #equals weight~height+height:group
anova(mod,modNoMainG)


## -----------------------------------------------------------------------------
car::Anova(mod,type=3)


## -----------------------------------------------------------------------------
mod0<-lm(weight~1)
anova(mod0,mod)


## -----------------------------------------------------------------------------
plot(mod,which=1)


## -----------------------------------------------------------------------------
new<-data.frame(group=c("A","B","A"),height=c(170,180,190))
new


## -----------------------------------------------------------------------------
pred<-predict(mod,newdata=new,interval="prediction")
cbind(new,pred)


## -----------------------------------------------------------------------------
pred2<-predict(mod,newdata=new,interval="confidence")
cbind(new,pred2)


## ----fig.show="hold"----------------------------------------------------------
pred.frame<-data.frame(group="A",height=seq(min(df$height),max(df$height)))
pc<-data.frame(predict(mod,newdata=pred.frame,interval="confidence"))
pp<-data.frame(predict(mod,newdata=pred.frame,interval="prediction"))
plot(pred.frame$height,pc[,1],col=2,type="l",xlab="height",ylab="predicted weight",main="Group A")
lines(pred.frame$height,pc[,2])
lines(pred.frame$height,pc[,3])
lines(pred.frame$height,pp[,2],lty=2)
lines(pred.frame$height,pp[,3],lty=2)
pred.frame<-data.frame(group="B",height=seq(min(df$height),max(df$height)))
pc<-data.frame(predict(mod,newdata=pred.frame,interval="confidence"))
pp<-data.frame(predict(mod,newdata=pred.frame,interval="prediction"))
plot(pred.frame$height,pc[,1],col=2,type="l",xlab="height",ylab="predicted weight",main="Group B")
lines(pred.frame$height,pc[,2])
lines(pred.frame$height,pc[,3])
lines(pred.frame$height,pp[,2],lty=2)
lines(pred.frame$height,pp[,3],lty=2)



## -----------------------------------------------------------------------------
Age <- c(18,23,25,35,65,54,34,56,72,19,23,42,18,39,37)
HR <- c(202,186,187,180,156,169,174,172,153,199,193,174,198,183,178)


## -----------------------------------------------------------------------------
modHR <- lm(HR~Age)
pred.frame<-data.frame(Age=18:72)
pp<-predict(modHR,interval="prediction",newdata=pred.frame)
pc<-predict(modHR,interval="confidence",newdata=pred.frame)
pred.age<-pred.frame$Age
plot(pred.age,pp[,1],lty=1,type="l",main="Prediction of Maximal Heart Rate",ylab="predicted",col=2,xlab="age")
lines(pred.age,pc[,2],lty=1)
lines(pred.age,pc[,3],lty=1)
lines(pred.age,pp[,2],lty=2)
lines(pred.age,pp[,3],lty=2)
grid()

