# Fitting non-linear kernels with support vector machines
# use polynomial, with degree, or radial kernels or gamma
# Import libraries
library(ISLR)
library(e1071)

# Set random seed for reproducability
set.seed(1)

# Generate non-linear class boundary data
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))


# View data
plot(x, col=y)

train=sample(200,100)
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
# 
# Call:
# svm(formula = y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, 
#     cost = 1)
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  radial 
#        cost:  1 
#       gamma:  1 
# 
# Number of Support Vectors:  37
# 
#  ( 17 20 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#  1 2
# 
# 
# 

ypred=predict(svmfit,dat[train,])
table(predict=ypred,dat[train,]$y)
#        
# predict  1  2
#       1 67  5
#       2  6 22

# Increase cost note an increased risk of overfitting
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=1,
           cost=1e5)
plot(svmfit,dat[train,])

tune.out=tune(svm,y~.,data=dat[train,],kernel="radial",
              ranges=list(cost=c(0.1,1,10,100,1000),
                          gamma=c(0.5,1,2,3,4)))

summary(tune.out)
# 
# Parameter tuning of ‘svm’:
# 
# - sampling method: 10-fold cross validation 
# 
# - best parameters:
#  cost gamma
#     1   0.5
# 
# - best performance: 0.13 
# 
# - Detailed performance results:
#     cost gamma error dispersion
# 1  1e-01   0.5  0.28 0.16865481
# 2  1e+00   0.5  0.13 0.10593499
# 3  1e+01   0.5  0.14 0.11737878
# 4  1e+02   0.5  0.18 0.10327956
# 5  1e+03   0.5  0.19 0.11005049
# 6  1e-01   1.0  0.30 0.15634719
# 7  1e+00   1.0  0.14 0.11737878
# 8  1e+01   1.0  0.18 0.10327956
# 9  1e+02   1.0  0.19 0.13703203
# 10 1e+03   1.0  0.22 0.11352924
# 11 1e-01   2.0  0.29 0.15951315
# 12 1e+00   2.0  0.15 0.11785113
# 13 1e+01   2.0  0.17 0.11595018
# 14 1e+02   2.0  0.19 0.11005049
# 15 1e+03   2.0  0.24 0.13498971
# 16 1e-01   3.0  0.29 0.15951315
# 17 1e+00   3.0  0.15 0.09718253
# 18 1e+01   3.0  0.17 0.11595018
# 19 1e+02   3.0  0.22 0.12292726
# 20 1e+03   3.0  0.25 0.12692955
# 21 1e-01   4.0  0.27 0.18287822
# 22 1e+00   4.0  0.16 0.09660918
# 23 1e+01   4.0  0.21 0.11972190
# 24 1e+02   4.0  0.21 0.07378648
# 25 1e+03   4.0  0.25 0.12692955
# 

table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,]))
#     pred
# true  1  2
#    1 72  5
#    2  7 16
