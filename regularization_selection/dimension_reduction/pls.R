# Partial Least Squares 
library(ISLR)
library(pls)

set.seed(1)

# Prepare data, split into train/test sets
Hitters=na.omit(Hitters)

x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

# Fit model
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE,validation="CV")


summary(pls.fit) # note lowest RMSE
# Data: 	X dimension: 131 19 
# 	Y dimension: 131 1
# Fit method: kernelpls
# Number of components considered: 19
# 
# VALIDATION: RMSEP
# Cross-validated using 10 random segments.
#        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
# CV           464.6    390.2    387.7    389.7    392.4    407.6    417.6
# adjCV        464.6    389.7    386.5    387.9    390.4    404.3    412.9
#        7 comps  8 comps  9 comps  10 comps  11 comps  12 comps  13 comps
# CV       418.4    412.3    401.1       396     397.1     400.9     399.9
# adjCV    413.0    407.7    397.4       392     393.2     396.8     395.8
#        14 comps  15 comps  16 comps  17 comps  18 comps  19 comps
# CV        397.5     401.0     402.1     401.2     400.2     403.8
# adjCV     393.6     396.8     397.7     397.0     396.1     399.4
# 
# TRAINING: % variance explained
#         1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
# X         38.12    53.46    66.05    74.49    79.33    84.56    87.09    90.74
# Salary    33.58    38.96    41.57    42.43    44.04    45.59    47.05    47.53
#         9 comps  10 comps  11 comps  12 comps  13 comps  14 comps  15 comps
# X         92.55     93.94     97.23     97.88     98.35     98.85     99.11
# Salary    48.42     49.68     50.04     50.54     50.78     50.92     51.04
#         16 comps  17 comps  18 comps  19 comps
# X          99.43     99.78     99.99    100.00
# Salary     51.11     51.15     51.16     51.18
# NULL

# Calculate MSE
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2) # [1] 101417.5

# with num components at 2 indicates by CV RMSE scores
pls.fit=plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
# Data: 	X dimension: 263 19 
# 	Y dimension: 263 1
# Fit method: kernelpls
# Number of components considered: 2
# TRAINING: % variance explained
#         1 comps  2 comps
# X         38.08    51.03
# Salary    43.05    46.40
# # NULL
