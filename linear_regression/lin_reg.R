library(MASS)
library(ISLR)

fix(Boston)         # view in row/col
# View Names

names(Boston)       # Use ?Boston
# [1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"
# [8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"

# Bind dataset variables to session
attach(Boston)

# define model
# lstat: lower status of the population %
# mdev: median value of owner-occupied homes $1000
lm.fit=lm(medv~lstat)

# Summary stats
summary(lm.fit)
# coef(lm.fit) # For just coefficients

# Call:
# lm(formula = medv ~ lstat)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -15.168  -3.990  -1.318   2.034  24.500
#
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 34.55384    0.56263   61.41   <2e-16 ***
# lstat       -0.95005    0.03873  -24.53   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.216 on 504 degrees of freedom
# Multiple R-squared:  0.5441,    Adjusted R-squared:  0.5432
# F-statistic: 601.6 on 1 and 504 DF,  p-value: < 2.2e-16

confint(lm.fit)

#                 2.5 %     97.5 %
# (Intercept) 33.448457 35.6592247
# lstat       -1.026148 -0.8739505

predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval="confidence")
#        fit      lwr      upr
# 1 29.80359 29.00741 30.59978
# 2 25.05335 24.47413 25.63256
# 3 20.30310 19.73159 20.87461

predict(lm.fit, data.frame(lstat=c(5,10,15)),
        interval="prediction")
#        fit       lwr      upr
# 1 29.80359 17.565675 42.04151
# 2 25.05335 12.827626 37.27907
# 3 20.30310  8.077742 32.52846

# draw the least squares regression line
plot(lstat,medv)
abline(lm.fit)

# adding additional lines
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(1:20, 1:20, pch=1:20)

# create multi plot view in 2x2 grid
par(mfrow=c(2,2))
plot(lm.fit)

# plot residuals and rstudent stats
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# compute leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
# 375       # index with the largest leverage stat
