# use lstat:black for interaction terms (all between)
# lstat*age interaction term as predictor

summary(lm(medv~lstat*age, data=Boston))

# Call:
# lm(formula = medv ~ lstat * age, data = Boston)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -15.806  -4.045  -1.333   2.085  27.552
#
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
# lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
# age         -0.0007209  0.0198792  -0.036   0.9711
# lstat:age    0.0041560  0.0018518   2.244   0.0252 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 6.149 on 502 degrees of freedom
# Multiple R-squared:  0.5557,    Adjusted R-squared:  0.5531
# F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16
