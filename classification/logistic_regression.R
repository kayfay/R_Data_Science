# logistic regression model
# setting family=binomial in the
# generalized linear regression model
library(MASS)
library(ISLR)
attach(Smarket)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket, family=binomial)
summary(glm.fits)$coef[,4]

# Call:
# glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 +
#     Volume, family = binomial, data = Smarket)
#
# Deviance Residuals:
#    Min      1Q  Median      3Q     Max
# -1.446  -1.203   1.065   1.145   1.326
#
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.126000   0.240736  -0.523    0.601
# Lag1        -0.073074   0.050167  -1.457    0.145
# Lag2        -0.042301   0.050086  -0.845    0.398
# Lag3         0.011085   0.049939   0.222    0.824
# Lag4         0.009359   0.049974   0.187    0.851
# Lag5         0.010313   0.049511   0.208    0.835
# Volume       0.135441   0.158360   0.855    0.392
#
# (Dispersion parameter for binomial family taken to be 1)
#
#     Null deviance: 1731.2  on 1249  degrees of freedom
# Residual deviance: 1727.6  on 1243  degrees of freedom
# AIC: 1741.6
#
# Number of Fisher Scoring iterations: 3


# pull just the coefficients
coef(glm.fits)

#  (Intercept)         Lag1         Lag2         Lag3         Lag4         Lag5
# -0.126000257 -0.073073746 -0.042301344  0.011085108  0.009358938  0.010313068
#       Volume
#  0.135440659

# view the pvalues
summary(glm.fits)$coef[,4]

# (Intercept)        Lag1        Lag2        Lag3        Lag4        Lag5
#   0.6006983   0.1452272   0.3983491   0.8243333   0.8514445   0.8349974
#      Volume
#   0.3924004

glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]

#           1           2           3           4           5           6
#  0.02833843 -0.07416246 -0.07548047  0.06090825  0.04313134  0.02782764
#           7           8           9          10
# -0.02939862  0.03692083  0.07048327 -0.04465630

# create vectors for class probabilities increase greater or less than 0.5
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>.5]="Up"

# create a confusoin matrix
table(glm.pred, Direction)

#         Direction
# glm.pred Down  Up
#     Down  145 141
#     Up    457 507

mean(glm.pred==Direction) # compute fraction of correct predicted days
# [1] 0.5216


# generate a realistic error rate and test model performance
train=(Year<2005)
Smarket.2005=Smarket[!train,]
Direction.2005=Direction[!train]

dim(Smarket.2005)
# [1] 252   9

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket, family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")

glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)

#         Direction.2005
# glm.pred Down Up
#     Down   77 97
#     Up     34 44

mean(glm.pred==Direction.2005) # .48
mean(glm.pred!=Direction.2005) # .52

# refitting with just lag1 and lag2 as pvalues were low
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,famliy=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)

#         Direction.2005
# glm.pred Down  Up
#     Down   35  35
#     Up     76 106

mean(glm.pred==Direction.2005) # .56 better

# predicting with new or changed data
predict(glm.fits,newdata=data.frame(
                                    Lag1=c(1.2,1.5), Lag2=c(1.1,-0,8))
                                    ,type="response")
