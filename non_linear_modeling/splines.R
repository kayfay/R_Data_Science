# Regression splines
library(ISLR)
library(splines)

# Data configure
attach(Wage)

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])

# basis functions for splines
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage) # 6 basis functions 1 intercept 7df
pred=predict(fit,newdata=list(age=age.grid),se=T)

# Plot data 
plot(age,wage,col="gray")

# Plot regression splines
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,df=6))
# [1] 3000    6
attr(bs(age,df=6),"knots")
#   25%   50%   75% 
# 33.75 42.00 51.00 

# Plot natural splines
fit2=lm(wage~ns(age,df=4),data=Wage) # fit natural spline
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid,pred2$fit,col="red",lwd=2)

# Fit smoothing splintes
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df # smoothing splines cv adjusted df
# [1] 6.794596

lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"),
       col=c("red","blue"),lty=1,lwd=2,cex=.8)


# Local regression
plot(age,wage,xlim)
