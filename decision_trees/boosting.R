# Fit boosted regression trees to the Boston dataset
library(MASS)
library(gbm)

set.seed(1)
pdf('boosting.pdf')
train=sample(1:nrow(Boston), nrow(Boston)/2)
boost.boston=gbm(medv~.,data=Boston[train,], distribution="gaussian",
                 n.trees=5000, interaction.depth=4)


summary(boost.boston)
#             var     rel.inf
# lstat     lstat 45.96792013
# rm           rm 31.22018272
# dis         dis  6.80567724
# crim       crim  4.07534048
# nox         nox  2.56586166
# ptratio ptratio  2.26983216
# black     black  1.78740116
# age         age  1.64495723
# tax         tax  1.36917603
# indus     indus  1.27052715
# chas       chas  0.80066528
# rad         rad  0.20727091
# zn           zn  0.01518785

par(mfrow=c(1,2))
plot(boost.boston, i='rm')
plot(boost.boston, i="lstat")

yhat.boost=predict(boost.boston, newdata=Boston[-train,],
                  n.trees=5000)
# MSE
mean((yhat.boost-Boston[-train,"medv"])^2)
# [1] 11.84694


dev.off()
