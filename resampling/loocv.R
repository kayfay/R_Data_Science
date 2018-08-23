# Leave-One-Out Cross-Validation
library(ISLR)
library(boot)

glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
# [1] 24.23151 24.23114

# fit poly-reg i1..i5
cv.error=rep(0,5)
for (i in 1:5){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
# error rates in MSE
