# non-linear transforms of predictor functions
# near-zero p values indicate good transforms

lm.fit2=lm(medv~lstat+I(lstat^2)) # I() for special characters
summary(lm.fit2)

# Call:
# lm(formula = medv ~ lstat + I(lstat^2))
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -15.2834  -3.8313  -0.5295   2.3095  25.4148
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 42.862007   0.872084   49.15   <2e-16 ***
# lstat       -2.332821   0.123803  -18.84   <2e-16 ***
# I(lstat^2)   0.043547   0.003745   11.63   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 5.524 on 503 degrees of freedom
# Multiple R-squared:  0.6407,    Adjusted R-squared:  0.6393
# F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16

# Analysis of Variance (anova) for futher quantification
anova(lm.fit, lm.fit2)

# Analysis of Variance Table
#
# Model 1: medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad +
#     tax + ptratio + black + lstat
# Model 2: medv ~ lstat + I(lstat^2)
#   Res.Df   RSS  Df Sum of Sq      F    Pr(>F)
# 1    492 11079
# 2    503 15347 -11   -4268.5 17.233 < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

par(mfrow=c(2,2))
plot(lm.fit2)

# fifth degree polynomial lstat^5 transform
lm.fit5=lm(medv~poly(lstat,5))
# summary(lm(medv~log(rm),data=Boston)) # as a log transform

# Call:
# lm(formula = medv ~ poly(lstat, 5))
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -13.5433  -3.1039  -0.7052   2.0844  27.1153
#
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)       22.5328     0.2318  97.197  < 2e-16 ***
# poly(lstat, 5)1 -152.4595     5.2148 -29.236  < 2e-16 ***
# poly(lstat, 5)2   64.2272     5.2148  12.316  < 2e-16 ***
# poly(lstat, 5)3  -27.0511     5.2148  -5.187 3.10e-07 ***
# poly(lstat, 5)4   25.4517     5.2148   4.881 1.42e-06 ***
# poly(lstat, 5)5  -19.2524     5.2148  -3.692 0.000247 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 5.215 on 500 degrees of freedom
# Multiple R-squared:  0.6817,    Adjusted R-squared:  0.6785
# F-statistic: 214.2 on 5 and 500 DF,  p-value: < 2.2e-16

