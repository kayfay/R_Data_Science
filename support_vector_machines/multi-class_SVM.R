# SVM with Multiple Classes using one-versus-one approach
# Import libraries
library(ISLR)
library(e1071)

# Set random seed for reproducability
set.seed(1)


# Generate three classes of data points
x=matrix(rnorm(200*2), ncol=2)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(rep(1,150),rep(2,50))
y=c(y, rep(0,50))
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit=svm(y~.,data=dat,kernel="radial",cost=10,gamma=1)
plot(svmfit,dat)
