# Perform ridge regression 
library(ISLR)
library(glmnet) # for ridge and lasso

set.seed(1)

# Omitting na values
Hitters=na.omit(Hitters)

# Create a matrix from the 19 predictors
# Transform qualitatiev variables into dummy variables
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary

# Ridge Regression mode 
grid=10^seq(10,-2,length=100) # for the lambda hyperparameter
ridge.mod=glmnet(x, y, alpha=0, lambda=grid)

# Coefficient predictors plus an intercept
# And 100 columns for each lambda value
dim(coef(ridge.mod)) # [1]  20 100

# Sum of squares for large lambda v small lambda
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # [1] 6.360612
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # [1] 57.11001

# Predicting coefficients using the model with lambda 50
predict(ridge.mod,s=50,type="coefficients")[1:20,]
#   (Intercept)         AtBat          Hits         HmRun          Runs 
#  4.876610e+01 -3.580999e-01  1.969359e+00 -1.278248e+00  1.145892e+00 
#           RBI         Walks         Years        CAtBat         CHits 
#  8.038292e-01  2.716186e+00 -6.218319e+00  5.447837e-03  1.064895e-01 
#        CHmRun         CRuns          CRBI        CWalks       LeagueN 
#  6.244860e-01  2.214985e-01  2.186914e-01 -1.500245e-01  4.592589e+01 
#     DivisionW       PutOuts       Assists        Errors    NewLeagueN 
# -1.182011e+02  2.502322e-01  1.215665e-01 -3.278600e+00 -9.496680e+00 

# Split train/test
train=sample(1:nrow(x), nrow(x)/2) # random vector
test=(-train)
y.test=y[test]

# Fit a ridge regression model
ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2) # [1] 121235.2
mean((mean(y[train])-y.test)^2) # [1] 226158.5

# Usign cross validation to choose the best hyperparameter lambda
cv.out=glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam = min(cv.out$lambda)

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2) # [1] 176177.7

# Refitting on the full dataset and comparing coefficients
out=glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]
