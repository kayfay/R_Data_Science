# Principal components regression
library(ISLR)
library(pls)

set.seed(2)

# Omit missing values
Hitters=na.omit(Hitters)

x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary

train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]


# Standardize predictors before generating princiapl components
# with scale and 10-fold cros val
pcr.fit=pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")

summary(pcr.fit)
# Data: 	X dimension: 263 19 
# 	Y dimension: 263 1
# Fit method: svdpc
# Number of components considered: 19
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
#        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
# CV             452    348.9    352.2    353.5    352.8    350.1    349.1
# adjCV          452    348.7    351.8    352.9    352.1    349.3    348.0
#        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
# CV       349.6    350.9    352.9     353.8     355.0     356.2     363.5
# adjCV    348.5    349.8    351.6     352.3     353.4     354.5     361.6
#        14 comps  15 comps  16 comps  17 comps  18 comps  19 comps
# CV        355.2     357.4     347.6     350.1     349.2     352.6
# adjCV     352.8     355.2     345.5     347.6     346.7     349.8
# 
# TRAINING: % variance explained
#         1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X         38.31    60.16    70.84    79.03    84.29    88.63    92.26    94.96
# Salary    40.63    41.58    42.17    43.22    44.90    46.48    46.69    46.75
#         9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X         96.28     97.26     97.98     98.65     99.15     99.47     99.75
# Salary    46.86     47.76     47.82     47.85     48.10     50.40     50.55
#         16 comps  17 comps  18 comps  19 comps
# X          99.89     99.97     99.99    100.00
# Salary     53.01     53.85     54.61     54.61
# NULL

# Find best model fit, predict, compute test MSE
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP") # 7
pcr.pred=predict(pcr.fit,x[test,], ncomp=7)

mean((pcr.pred-y.test)^2) # [1] 142274.4

# Fit on full dataset with M = 7
pcr.fit=pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)
# Data: 	X dimension: 263 19 
# 	Y dimension: 263 1
# Fit method: svdpc
# Number of components considered: 7
# TRAINING: % variance explained
#    1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps
# X    38.31    60.16    70.84    79.03    84.29    88.63    92.26
# y    40.63    41.58    42.17    43.22    44.90    46.48    46.69
# NULL
