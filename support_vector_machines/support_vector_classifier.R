# With kernel linear, performing support vector classifier
# with the svm package
# a cost argument specifies a violation to the margin
# a large cost results in narrow margins with few support vectors
# on or violating the margin

# Import libraries
library(ISLR)
library(e1071)

# Set random seed for reproducability
set.seed(1)

# Generate a dataset
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)

# Plot support vector classifier
plot(svmfit, dat)

# Check support vector identities
svmfit$index
# [1]  1  2  5  7 14 16 17

summary(svmfit)
# 
# Call:
# svm(formula = y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  linear 
#        cost:  10 
#       gamma:  0.5 
# 
# Number of Support Vectors:  7
# 
#  ( 4 3 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#  -1 1
# 
# 
# 

# Using a lower cost value to generate more support vectors
svmfit=svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)

# Tune the hyperparameters using 10 fold cross val
tune.out = tune(svm,y~.,data=dat, kernel="linear",
                ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))

summary(tune.out)
# 
# Parameter tuning of ‘svm’:
# 
# - sampling method: 10-fold cross validation 
# 
# - best parameters:
#  cost
#   0.1
# 
# - best performance: 0.15 
# 
# - Detailed performance results:
#    cost error dispersion
# 1 1e-03  0.55  0.4377975
# 2 1e-02  0.55  0.4377975
# 3 1e-01  0.15  0.3374743  # 0.01 best error / dispersion
# 4 1e+00  0.15  0.2415229
# 5 5e+00  0.25  0.3535534
# 6 1e+01  0.25  0.3535534
# 7 1e+02  0.25  0.3535534
# 

# Using the best model
bestmod=tune.out$best.model
summary(bestmod)
# 
# Call:
# best.tune(method = svm, train.x = y ~ ., data = dat, ranges = list(cost = c(0.001, 
#     0.01, 0.1, 1, 5, 10, 100)), kernel = "linear")
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  linear 
#        cost:  0.1 
#       gamma:  0.5 
# 
# Number of Support Vectors:  16
# 
#  ( 8 8 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#  -1 1
# 
# 
# 

# Generate test dataset
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,]+1
testdat=data.frame(x=xtest,y=as.factor(ytest))

ypred=predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)
#        truth
# predict -1  1
#      -1 10  2
#      1   2  6

# Modify for linear sepearability
x[y==1,]=x[y==1,]+0.5
plot(x, col=(y+5)/2, pch=19)

# fit with large cost
dat=data.frame(x=x,y=as.factor(y))
svmfit=svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
# 
# Call:
# svm(formula = y ~ ., data = dat, kernel = "linear", cost = 1e+05)
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  linear 
#        cost:  1e+05 
#       gamma:  0.5 
# 
# Number of Support Vectors:  3
# 
#  ( 1 2 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#  -1 1
# 
# 
# Plot
plot(svmfit, dat)
ypred=predict(svmfit,dat)
table(predict=ypred,truth=dat$y)
#        truth
# predict -1  1
#      -1 10  0
#      1   0 10

# Try with smaller 
svmfit=svm(y~.,data=dat,kernel="linear",cost=1)
summary(svmfit)
# 
# Call:
# svm(formula = y ~ ., data = dat, kernel = "linear", cost = 1)
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  linear 
#        cost:  1 
#       gamma:  0.5 
# 
# Number of Support Vectors:  7
# 
#  ( 4 3 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#  -1 1
# 
# 
# 
# For wider support vectors
plot(svmfit, dat)
ypred=predict(svmfit,dat)
table(predict=ypred,truth=dat$y)
#        truth
# predict -1  1
#      -1 10  1 # Note misprediction
#      1   0  9

