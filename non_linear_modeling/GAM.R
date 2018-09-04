# Generalized Additive Models
library(ISLR)
library(splines)
library(gam)

attach(Wage)

# GAM using linear regression and natural splines
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
# GAM using smoothing splines
gam.m3 = gam(wage~s(year,4)+s(age,5)+education,data=Wage)

# Plots
par(mfrow=c(1,3))
plot(gam.m3,se=TRUE,col="blue")

plot.Gam(gam1, se=TRUE, col="red")

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
# Analysis of Deviance Table
# 
# Model 1: wage ~ s(age, 5) + education
# Model 2: wage ~ year + s(age, 5) + education
# Model 3: wage ~ s(year, 4) + s(age, 5) + education
#   Resid. Df Resid. Dev Df Deviance       F    Pr(>F)    
# 1      2990    3711731                                  
# 2      2989    3693842  1  17889.2 14.4771 0.0001447 ***
# 3      2986    3689770  3   4071.1  1.0982 0.3485661    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Note a lm not including year performs better

summary(gam.m3)
# 
# Call: gam(formula = wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
# Deviance Residuals:
#     Min      1Q  Median      3Q     Max 
# -119.43  -19.70   -3.33   14.17  213.48 
# 
# (Dispersion Parameter for gaussian family taken to be 1235.69)
# 
#     Null Deviance: 5222086 on 2999 degrees of freedom
# Residual Deviance: 3689770 on 2986 degrees of freedom
# AIC: 29887.75 
# 
# Number of Local Scoring Iterations: 2 
# 
# Anova for Parametric Effects
#              Df  Sum Sq Mean Sq F value    Pr(>F)    
# s(year, 4)    1   27162   27162  21.981 2.877e-06 ***
# s(age, 5)     1  195338  195338 158.081 < 2.2e-16 ***
# education     4 1069726  267432 216.423 < 2.2e-16 ***
# Residuals  2986 3689770    1236                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Anova for Nonparametric Effects
#             Npar Df Npar F  Pr(F)    
# (Intercept)                          
# s(year, 4)        3  1.086 0.3537    
# s(age, 5)         4 32.380 <2e-16 ***
# education                            
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Predictions
preds=predict(gam.m2,newdata=Wage)

# local regression lo()
gam.lo = gam(wage~s(year,df=4)+lo(age,span=0.7)+education, data=Wage)
plot.Gam(gam.lo, se=TRUE, col="green")

gam.lo.i=gam(wave~lo(year,age,span=0.5)+education,data=Wage)
plot.Gam(gam.lo.i, se=TRUE, col="green")

# Fit logistic regresion GAM
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

table(education,I(wage>250))
#                     
# education            FALSE TRUE
#   1. < HS Grad         268    0
#   2. HS Grad           966    5
#   3. Some College      643    7
#   4. College Grad      663   22
#   5. Advanced Degree   381   45


gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,
           family=binomial,data=Wage,
           subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")
