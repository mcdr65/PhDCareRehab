library(psych)
d.wide  <-  read.csv("https://raw.githubusercontent.com/mcdr65/StatsRsource/master/Data/omega.csv")
str(d.wide)
## data.frame(names(d.wide))
## BL<- seq(13,187,by=5)
## fact <- c(1,2,3,5,8,11)
cont <- c(4,6,7,9,10,12)
by(d.wide$age,d.wide$group,describe)
by(d.wide$gender,d.wide$group,table)
describeBy(d.wide[,cont],group=d.wide$group,skew=FALSE)

describeBy(d.wide[,fac],group=d.wide$group,skew=FALSE)
d.s<-split(d.wide[,fact],f=d.wide$group)
lapply(d.s,summary)





