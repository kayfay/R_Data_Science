# Fit a regression tree to the Boston dataset
library(tree)
library(MASS)

# Crete dataset
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
# Fit model
tree.boston=tree(medv~.,Boston,subset=train)

summary(tree.boston)
# 
# Regression tree:
# tree(formula = medv ~ ., data = Boston, subset = train)
# Variables actually used in tree construction:
# [1] "lstat" "rm"    "dis"   "nox"  
# Number of terminal nodes:  10 
# Residual mean deviance:  13.27 = 3225 / 243 
# Distribution of residuals:
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -16.5200  -2.1830   0.1659   0.0000   1.9360  13.2700 

pdf('regress_trees.pdf')

# Plot tree
plot(tree.boston)
text(tree.boston,pretty=0)

# Cross validation pruning
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

# Prune
prune.boston=prune.tree(tree.boston, best=5)
plot(prue.boston)
text(prune.boston, pretty=0)

# predict using unpruned tree with cv results
yhat=predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train, "medv"]
plot(yhat,boston.test)
abline(0,1)

#  Mean squared error rate
mean((yhat-boston.test)^2)
# [1] 25.04559

dev.off()
