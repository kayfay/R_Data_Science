# Creating reciever operating characteristic curves
# for visulization of classification metrics

library(ISLR)
library(ROCR)
library(e1071)

# Generate data
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
train=sample(200,100)



# Function for plotting ROC curves
rocplot=function(pred,truth,...) {
  predob = prediction(pred,truth)
  pref = performance(predob, "tpr", "fpr")
  plot(pref,...)}

# Fit svm and extract fitted values
svmfit.opt=svm(y~.,data=dat[train,],kernel="radial",
               gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],
                          decision.values=TRUE))$decision.values

# Plot ROC plot
par(mfrow=c(1,2))
rocplot(fitted,dat[train,"y"],main="Training Data")

# Genearte a more flexable fit
svmfit.flex=svm(y~.,data=dat[train,],kernel="radial",
                gamma=50,cost=1,decision.values=T)

# Fit svm and extract fitted values
fitted=attributes(predict(svmfit.flex,dat[train,],
                          decision.values=T))$decision.values

# plot ROC plot
rocplot(fitted,dat[train,"y"],add=T,col="red")

# plot based on test data
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")
