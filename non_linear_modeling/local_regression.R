# Regression splines
library(ISLR)
library(splines)

# Data configure
attach(Wage)

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])


# Local regression
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Spam=0.5"),
       col=c("red","blue"),lty=1,lwd=2,cex=.8)
