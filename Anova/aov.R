## ----chunk_setup, include=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(echo = T, out.width="49%",message=F, warning=F, comment=NA,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(digits=5,show.signif.stars=FALSE),dev.args=list(bg = 'transparent'))


## ----needed,echo=TRUE,results="hide"------------------------------------------
library(psych)
library(emmeans)


## -----------------------------------------------------------------------------
Bef <- c(20, 12, 18, 14, 16, 21, 17, 13, 18, 21, 13, 12, 15, 17, 16, 17,  9, 10, 15,  8,  8, 11, 13, 14)
treat<-as.factor(c(rep("MP",10),rep("MA",6),rep("M",8)))
data<-data.frame(Bef,treat)
headTail(data)
str(data)


## -----------------------------------------------------------------------------
boxplot(Bef~treat,data=data)
describeBy(data,group=data$treat,mat=FALSE)


## -----------------------------------------------------------------------------
mod  <- lm(Bef~treat,data) 


## -----------------------------------------------------------------------------
mod0 <- lm(Bef~1,data)


## -----------------------------------------------------------------------------
anova(mod0,mod)


## -----------------------------------------------------------------------------
(RSS <-sum(residuals(mod)^2))
(SS  <-sum(residuals(mod0)^2))
((SS-RSS)/SS) ##known as R^2


## -----------------------------------------------------------------------------
(df2<-mod$df.residual) ##numerator df
(df1<-mod0$df.residual-mod$df.residual) ##denominator df


## -----------------------------------------------------------------------------
F <- (SS-RSS)/(df1)/(RSS/(df2))
p <- 1-pf(F,df1=df1,df2=df2)
sigma.mod <- sqrt(RSS/df2)
print(data.frame(SS,RSS,ESS=SS-RSS,F,p,sigma.mod),row.names=FALSE)


## -----------------------------------------------------------------------------
summary(mod)
confint(mod)
#summary(mod0) ## The only-intercept model, uncomment if you want to look at it.


## -----------------------------------------------------------------------------
emmeans(mod,specs=pairwise~treat,infer=TRUE) ##Estimated marginal means


## ----fig.show="hold"----------------------------------------------------------
plot(mod,which=c(1,2))


## -----------------------------------------------------------------------------
modc <-aov(Bef~treat,data)
summary(modc)
TukeyHSD(modc)
plot(TukeyHSD(modc))


## -----------------------------------------------------------------------------
nage <- 3
ntherapy <- 2
nsample <- 100
n <- nage * nsample* ntherapy
age <- gl(n = nage, k = nsample, length = n,labels=c("child","young","old"))
therapy <- gl(n = ntherapy, k = nsample, length = n,labels=c("Ctrl","Trt"))
mu <- 40
alpha <- c(1, 1) 
beta <- c(1)
gamma <- c(-3,3)
parameter <- c(mu, alpha, beta, gamma)
sigma <- 12
set.seed(9)
eps <- rnorm(n, 0, sigma)
X <- as.matrix(model.matrix(~ age*therapy) ) 
response <- as.numeric(as.matrix(X) %*% as.matrix(parameter) + eps)
d.cross<-data.frame(response,age,therapy)


## -----------------------------------------------------------------------------
headTail(d.cross)


## -----------------------------------------------------------------------------
with(d.cross,xtabs(~age+therapy))

## -----------------------------------------------------------------------------
aggregate(response~therapy+age,data=d.cross,summary)
with(d.cross,interaction.plot(x.factor=age,trace.factor=therapy,response=response,trace.label="treatment",xlab="age group",ylab="mean of response"))


## -----------------------------------------------------------------------------
model2f <-lm(response~age*therapy,data=d.cross)
summary(model2f)


## -----------------------------------------------------------------------------
parameter


## -----------------------------------------------------------------------------
anova(model2f)
summary(aov(response~age*therapy,data=d.cross)) ## equivalent!


## -----------------------------------------------------------------------------
summary(aov(response~therapy*age,data=d.cross)) ## equivalent!


## -----------------------------------------------------------------------------
library(car)
Anova(model2f,type=3)

