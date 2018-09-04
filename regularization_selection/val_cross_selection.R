# Choosing models using the validation set and a cross-validation set approach
library(ISLR)
library(leaps)
set.seed(1)

# Remove NA val
Hitters=na.omit(Hitters)

# Create random indexes for train and test splits
train=sample(c(TRUE,FALSE), nrow(Hitters), rep=TRUE)
test=(!train)

# Perform best model selection from subsets
regfit.best=regsubsets(Salary~.,data=Hitters[train,], nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,]) # build an X matrix fom data

# extract coefficient for the best models
# for each i coefficient multiply into the columns of X
# resulting in predictions, and compute test MSE
val.errors=rep(NA,19)
for(i in 1:19) {
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}

# Results
best.model = which.min(val.errors) # 10
val.errors[best.model] # [1] 148162.1
coef(regfit.best, 10)
# (Intercept)       AtBat        Hits       Walks      CAtBat       CHits      CHmRun      CWalks     LeagueN 
# -80.2751499  -1.4683816   7.1625314   3.6430345  -0.1855698   1.1053238   1.3844863  -0.7483170  84.5576103 
#   DivisionW     PutOuts 
# -53.0289658   0.2381662 

# Function for the above process
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


# Variable selection on full data set varies 
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
#  (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI       CWalks    DivisionW 
#  162.5354420   -2.1686501    6.9180175    5.7732246   -0.1300798    1.4082490    0.7743122   -0.8308264 -112.3800575 
#      PutOuts      Assists 
#    0.2973726    0.2831680 

# Trying on different model sizes with cross validation
k = 10 # k training set folds
folds=sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# Perform cross-val
for(j in 1:k) {
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19) {
    pred=predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean) # average over columns
best.xval = min(mean.cv.errors) # 114275.4

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b') # 11 variable model looks best

# Crossval shows best sample selection with coef with 11 variables
reg.best = regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(reg.best, 11)
#  (Intercept)        AtBat         Hits        Walks       CAtBat        CRuns         CRBI       CWalks      LeagueN 
#  135.7512195   -2.1277482    6.9236994    5.6202755   -0.1389914    1.4553310    0.7852528   -0.8228559   43.1116152 
#    DivisionW      PutOuts      Assists 
# -111.1460252    0.2894087    0.2688277 

