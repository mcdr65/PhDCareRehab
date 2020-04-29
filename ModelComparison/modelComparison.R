## ----chunk_setup, include=FALSE, eval=TRUE------------------------------------
knitr::opts_chunk$set(echo = T, out.width="49%",message=F, warning=F, comment=NA,
                      eval=T, cache.rebuild=F, cache=F, R.options=list(digits=5,show.signif.stars=FALSE),dev.args=list(bg = 'transparent'))


## -----------------------------------------------------------------------------
library(psych)
n <- 20
alpha <- 2
beta <- 3
sigma <-10
set.seed(10)
group <- as.factor(sample(c(0,1),n,replace=TRUE))
Y <- alpha+beta*(group==1)+rnorm(n,0,sigma)
headTail(data.frame(Y,group))


## -----------------------------------------------------------------------------
summary(Y)
by(Y,group,summary)
boxplot(Y~group)


## -----------------------------------------------------------------------------
t.test(Y~group,var.equal=TRUE)


## -----------------------------------------------------------------------------
mod <- lm(Y~group) 
summary(mod)
confint(mod)


## -----------------------------------------------------------------------------
mod0 <- lm(Y~1) 
summary(mod0)


## -----------------------------------------------------------------------------
anova(mod0,mod)


## -----------------------------------------------------------------------------
(RSS <-sum(mod$residuals^2))
(RSS0 <-sum(mod0$residuals^2))
(RSS0-RSS)


## -----------------------------------------------------------------------------
(RSS0-RSS)/RSS0


## -----------------------------------------------------------------------------
(df <- n-2)
(df0 <- n-1)


## -----------------------------------------------------------------------------
F <- (RSS0-RSS)/(df0-df)/(RSS/(df))
p <- 1-pf(F,df1=df0-df,df2=df)
sigma <- sqrt(RSS/df)
sigma0 <- sqrt(RSS0/df0)
print(data.frame(RSS0,RSS,SSExplained=RSS0-RSS,F,p,sigma,sigma0),row.names=FALSE)


## ----eval=FALSE,echo=FALSE----------------------------------------------------
## s <- summary(mod)$sigma
## s0 <- summary(mod0)$sigma
## sum(log(dnorm(Y,predict(mod),sqrt(s^2*(n-1)/n))))
## sum(log(dnorm(Y,predict(mod0),s0)))

## -----------------------------------------------------------------------------
logLik(mod)
logLik(mod0)


## -----------------------------------------------------------------------------
AIC(mod0,mod)
BIC(mod0,mod)

