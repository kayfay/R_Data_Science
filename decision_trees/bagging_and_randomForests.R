# Apply bagging and random forests on boston data
library(randomForest)
library(MASS)

# Create sets of data and train initial model
set.seed(1)
pdf('bagging_randomForests.pdf')
train=sample(1:nrow(Boston), nrow(Boston)/2)
boston.test=Boston[-train,"medv"]
# with mtry try all m 13 predictors for bagging regression
bag.boston=randomForest(medv~.,data=Boston,subset=train,
                        mtry=13,importance=TRUE)
bag.boston
# 
# Call:
#  randomForest(formula = medv ~ ., data = Boston, mtry = 13, importance = TRUE,      subset = train) 
#                Type of random forest: regression
#                      Number of trees: 500
# No. of variables tried at each split: 13
# 
#           Mean of squared residuals: 10.80817
#                     % Var explained: 86.91

# Predict, plot, metrics model
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
# Mean squared error
mean((yhat.bag-boston.test)^2)
# [1] 13.444

# Growing with ntree increased to 25
bag.boston=randomForest(medv~.,data=Boston,subset=train,
                        mtry=13,ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
# [1] 14.73186

# Use random forest with mtry = 6
rf.boston=randomForest(medv~.,data=Boston,subset=train,
                       mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
# [1] 11.68512

# view importance of each variable
importance(rf.boston)
#           %IncMSE IncNodePurity
# crim    13.479372    1119.35537
# zn       4.418420      44.50284
# indus   11.948527    1052.43706
# chas     3.372582      54.10144
# nox     10.758730    1070.39096
# rm      31.779074    6073.56242
# age     10.753837     514.91530
# dis     14.675687    1295.05326
# rad      3.258615      92.97208
# tax      8.192236     395.62947
# ptratio 10.658047     850.29370
# black    6.698559     342.37440
# lstat   30.465723    7603.38870

# Plots for node impurty or RSS
varImpPlot(rf.boston)
dev.off()
