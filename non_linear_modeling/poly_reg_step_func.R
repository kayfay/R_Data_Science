# Using wage data for illustrating non-linear fitting procedures
library(ISLR)
attach(Wage)

# Fit polynomial model
fit=lm(wage~poly(age,4, raw=T),data=Wage)  
coef(summary(fit)) # polynomial degrees to 4th deg
#                 Estimate Std. Error    t value     Pr(>|t|)
# (Intercept)    111.70361  0.7287409 153.283015 0.000000e+00
# poly(age, 4)1  447.06785 39.9147851  11.200558 1.484604e-28
# poly(age, 4)2 -478.31581 39.9147851 -11.983424 2.355831e-32
# poly(age, 4)3  125.52169 39.9147851   3.144742 1.678622e-03
# poly(age, 4)4  -77.91118 39.9147851  -1.951938 5.103865e-02

# Grid for predictions on age ranges
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)

# Plot data
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue") # predictions
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) 

# ANOVA for comparing 5 models
fit.1=lm(wage~age,date=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# Analysis of Variance Table
# 
# Model 1: wage ~ age
# Model 2: wage ~ poly(age, 2)
# Model 3: wage ~ poly(age, 3)
# Model 4: wage ~ poly(age, 4)
# Model 5: wage ~ poly(age, 5)
#   Res.Df     RSS Df Sum of Sq        F    Pr(>F)    
# 1   2998 5022216                                    
# 2   2997 4793430  1    228786 143.5931 < 2.2e-16 ***
# 3   2996 4777674  1     15756   9.8888  0.001679 ** 
# 4   2995 4771604  1      6070   3.8098  0.051046 .  
# 5   2994 4770322  1      1283   0.8050  0.369682    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#  Note a cubic or quartic polynomial model have p values with reasonable fits

coef(summary(fit.5)) # Using the poly^5 orthagonal matrix
#                 Estimate Std. Error     t value     Pr(>|t|)
# (Intercept)    111.70361  0.7287647 153.2780243 0.000000e+00
# poly(age, 5)1  447.06785 39.9160847  11.2001930 1.491111e-28
# poly(age, 5)2 -478.31581 39.9160847 -11.9830341 2.367734e-32
# poly(age, 5)3  125.52169 39.9160847   3.1446392 1.679213e-03
# poly(age, 5)4  -77.91118 39.9160847  -1.9518743 5.104623e-02
# poly(age, 5)5  -35.81289 39.9160847  -0.8972045 3.696820e-01

# Use polynomial logistic regression for wages more than 250k
fit=glm(I(wage>250)~poly(age,4),data=Wage)
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)
pfit=exp(preds$fit)/(1+exp(preds$fit))
se.bands.logit=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
se.bands=exp(se.bands.logit)/(1+exp(se.bands.logit))

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgray")
lines(age.grid,pfit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# Fitting a step function
table(cut(age,4)) # create bins
# 
# (17.9,33.5]   (33.5,49]   (49,64.5] (64.5,80.1] 
#         750        1399         779          72 

fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))
#                         Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)            94.158392   1.476069 63.789970 0.000000e+00
# cut(age, 4)(33.5,49]   24.053491   1.829431 13.148074 1.982315e-38
# cut(age, 4)(49,64.5]   23.664559   2.067958 11.443444 1.040750e-29
# cut(age, 4)(64.5,80.1]  7.640592   4.987424  1.531972 1.256350e-01
