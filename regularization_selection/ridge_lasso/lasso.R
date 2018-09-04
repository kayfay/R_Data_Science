# Perform Lasso
library(ISLR)
library(glmnet) # for ridge and lasso

set.seed(1)

# Omitting na values
Hitters=na.omit(Hitters)

# Create a matrix from the 19 predictors
# Transform qualitatiev variables into dummy variables
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary

# Split train/test
train=sample(1:nrow(x), nrow(x)/2) # random vector
test=(-train)
y.test=y[test]

grid=10^seq(10,-2,length=100) # for the lambda hyperparameter
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

# Plot with cross validation
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam=min(cv.out$lambda)

lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) # [1] 110507.6

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]

