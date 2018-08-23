# Resampling using cross validation

library(ISLR)
set.seed(1)
train=sample(392,196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# [1] 26.14142 # Test MSE

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
# [1] 19.82259 # MSE

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
# [1] 19.78252 # MSE
