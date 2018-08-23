# The Bootstrap
library(ISLR)
library(boot)

alpha.fn=function(data,index){
    X=data$X[index]
    Y=data$Y[index]
    return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio,1:100)
# [1] 0.5758321

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
# [1] 0.5963833

# automated approach
boot(Portfolio,alpha.fn,R=1000)

# ORDINARY NONPARAMETRIC BOOTSTRAP
#
#
# Call:
# boot(data = Portfolio, statistic = alpha.fn, R = 1000)
#
#
# Bootstrap Statistics :
#      original        bias    std. error
# t1* 0.5758321 -7.315422e-05  0.08861826

# note alpha  0.5758, SE(alpha_estimate) = 0.0886
