# Use classification tree based models on carseat sales

library(tree)
library(ISLR)

attach(Carseats)

# Variable High if Sales exceeds 8
High=ifelse(Sales<=8, "No", "Yes")

# Bind the carseat dataframe to the classification variable
Carseats = data.frame(Carseats, High)

# Evaluate the tree model
tree.carseats=tree(High~.-Sales,Carseats)

# Plot
pdf('class_trees.pdf')
summary(tree.carseats)
# 
# Classification tree:
# tree(formula = High ~ . - Sales, data = Carseats)
# Variables actually used in tree construction:
# [1] "ShelveLoc"   "Price"       "Income"      "CompPrice"   "Population" 
# [6] "Advertising" "Age"         "US"         
# Number of terminal nodes:  27 
# Residual mean deviance:  0.4575 = 170.7 / 373 
# Misclassification error rate: 0.09 = 36 / 400 

# Residual mean deviance reported by -2 \sum_m \sum_k n_mk log \hatp_mk
# Or 
# n - \abs{T_0} / deviance (400 - 27 = 373)
# 

plot(tree.carseats)
text(tree.carseats, pretty=0)

# Text representation
tree.carseats
# node), split, n, deviance, yval, (yprob)
#       * denotes terminal node
# 
#   1) root 400 541.500 No ( 0.59000 0.41000 )  
#     2) ShelveLoc: Bad,Medium 315 390.600 No ( 0.68889 0.31111 )  
#       4) Price < 92.5 46  56.530 Yes ( 0.30435 0.69565 )  
#         8) Income < 57 10  12.220 No ( 0.70000 0.30000 )  

# Evaulate performance
set.seed(2)
# Create training set and test set by sampling
train=sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test=High[-train]
# Create model on training set subset
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
# Create predictions with classification tree
tree.pred=predict(tree.carseats,Carseats.test,type="class")

# create table
table(tree.pred,High.test)
#          High.test
# tree.pred No Yes
#       No  86  27
#       Yes 30  57
(86+57)/200
# [1] 0.715 % correct predictions

# Cross validation for tree complexity pruning
# Prune with missclassification error rate
set.seed(3)
(cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass))
# $size # number of terminal nodes
# [1] 19 17 14 13  9  7  3  2  1
# 
# $dev # Cross validation error rate
# [1] 60 63 66 64 62 62 74 62 82
# 
# $k # alpha, cost-complexity parameter
# [1]       -Inf  0.0000000  0.6666667  1.0000000  1.7500000  2.0000000  4.2500000
# [8]  5.0000000 23.0000000
# 
# $method
# [1] "misclass"
# 
# attr(,"class")
# [1] "prune"         "tree.sequence"
names(cv.carseats)
# [1] "size"   "dev"    "k"      "method"

par(mfrow=c(1,2))
# Plot as a function of size and error rate
# and k cross validation error rate
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

# Prune to obtain the nine-node tree
prune.carseats=prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# Predict based on pruned tree
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred, High.test)
#          High.test
# tree.pred No Yes
#       No  94  24
#       Yes 22  60
(94+60)/200 # [1] 0.77


dev.off()
