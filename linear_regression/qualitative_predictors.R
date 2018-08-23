# Handling qualitative predictors
# Shelveloc creation of dummy variables

library(ISLR)
library(MASS)
fix(Carseats)
names(Carseats)
#  [1] "Sales"       "CompPrice"   "Income"      "Advertising" "Population"
#  [6] "Price"       "ShelveLoc"   "Age"         "Education"   "Urban"
# [11] "US"


lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)

# Call:
# lm(formula = Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -2.9208 -0.7503  0.0177  0.6754  3.3413
#
# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)         6.5755654  1.0087470   6.519 2.22e-10 ***
# CompPrice           0.0929371  0.0041183  22.567  < 2e-16 ***
# Income              0.0108940  0.0026044   4.183 3.57e-05 ***
# Advertising         0.0702462  0.0226091   3.107 0.002030 **
# Population          0.0001592  0.0003679   0.433 0.665330
# Price              -0.1008064  0.0074399 -13.549  < 2e-16 ***
# ShelveLocGood       4.8486762  0.1528378  31.724  < 2e-16 ***
# ShelveLocMedium     1.9532620  0.1257682  15.531  < 2e-16 ***
# Age                -0.0579466  0.0159506  -3.633 0.000318 ***
# Education          -0.0208525  0.0196131  -1.063 0.288361
# UrbanYes            0.1401597  0.1124019   1.247 0.213171
# USYes              -0.1575571  0.1489234  -1.058 0.290729
# Income:Advertising  0.0007510  0.0002784   2.698 0.007290 **
# Price:Age           0.0001068  0.0001333   0.801 0.423812
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 1.011 on 386 degrees of freedom
# Multiple R-squared:  0.8761,    Adjusted R-squared:  0.8719
# F-statistic:   210 on 13 and 386 DF,  p-value: < 2.2e-16

attach(Carseats)
contrasts(ShelveLoc)

#        Good Medium
# Bad       0      0
# Good      1      0
# Medium    0      1
