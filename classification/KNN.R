# K-Nearest neighbors
library(ISLR)
library(class)
attach(Smarket)

train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

train.X=cbind(Lag1, Lag2)[train,]
test.X=cbind(Lag1, Lag2)[!train,]
train.Direction=Direction[train]

set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)

#         Direction.2005
# knn.pred Down Up
#     Down   43 58
#     Up     68 83

# (83+42)/252 # as .5 % with k=1

knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)

#         Direction.2005
# knn.pred Down Up
#     Down   48 54
#     Up     63 87

mean(knn.pred==Direction.2005) # .54
