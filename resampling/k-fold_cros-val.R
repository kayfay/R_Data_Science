# k-Fold Cross-Validation
library(ISLR)
library(boot)

set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}

cv.error.10
# MSE scores for 10-fold
#  [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609 19.71201
#  [9] 18.95140 19.50196
