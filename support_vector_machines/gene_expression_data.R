# Khan data set consists of tissue samples for four
# types of small round blue cell tumors
# use expression measurements to predict cancer subtypes
library(ISLR)
library(e1071)
names(Khan)
# [1] "xtrain" "xtest"  "ytrain" "ytest" 
dim(Khan$xtest) # 2,308 gene expression measurements
# [1]   20 2308
length(Khan$ytrain) # 63 observations
# [1] 63
length(Khan$ytest) # 20 observations
# [1] 20
table(Khan$ytrain) # 4 gene labels in labeled training set
# 
#  1  2  3  4 
#  8 23 12 20 
table(Khan$ytest) # 4 gene labels in labeled test set 
# 
# 1 2 3 4 
# 3 6 6 5 
# large number of features relative to the number of observations
# a linear kernel may be better as unneeded flexability
dat=data.frame(x=Khan$xtrain,y=as.factor(Khan$ytrain))
out=svm(y~.,data=dat,kernel="linear",cost=10)
summary(out)
# 
# Call:
# svm(formula = y ~ ., data = dat, kernel = "linear", cost = 10)
# 
# 
# Parameters:
#    SVM-Type:  C-classification 
#  SVM-Kernel:  linear 
#        cost:  10 
#       gamma:  0.0004332756 
# 
# Number of Support Vectors:  58
# 
#  ( 20 20 11 7 )
# 
# 
# Number of Classes:  4 
# 
# Levels: 
#  1 2 3 4
# 
# 
# 
table(out$fitted,dat$y) # Note no errors on the training data
#    
#      1  2  3  4
#   1  8  0  0  0
#   2  0 23  0  0
#   3  0  0 12  0
#   4  0  0  0 20

dat.te=data.frame(x=Khan$xtest,y=as.factor(Khan$ytest))
pred.te=predict(out,newdata=dat.te)
table(pred.te,dat.te$y) # Note no errors on the test set
#        
# pred.te 1 2 3 4
#       1 3 0 0 0
#       2 0 6 2 0
#       3 0 0 4 0
#       4 0 0 0 5

# Note a cost of 10 yields no training errors
