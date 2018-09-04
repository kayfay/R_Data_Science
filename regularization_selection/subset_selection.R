# Predict determining subset selection
# Import libraries
library(ISLR)
library(leaps)

fix(Hitters)
#                    AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun
# -Andy Allanson       293   66     1   30  29    14     1    293    66      1
# -Alan Ashby          315   81     7   24  38    39    14   3449   835     69
# -Alvin Davis         479  130    18   66  72    76     3   1624   457     63

names(Hitters)
#  [1] "AtBat"     "Hits"      "HmRun"     "Runs"      "RBI"       "Walks"    
#  [7] "Years"     "CAtBat"    "CHits"     "CHmRun"    "CRuns"     "CRBI"     
# [13] "CWalks"    "League"    "Division"  "PutOuts"   "Assists"   "Errors"   
# [19] "Salary"    "NewLeague"

dim(Hitters)
# [1] 322  20

sum(is.na(Hitters$Salary))
# [1] 59

# Omitting na values
Hitters=na.omit(Hitters)
dim(Hitters)
# [1] 263  20

sum(is.na(Hitters))
# [1] 0

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
# Subset selection object
# Call: regsubsets.formula(Salary ~ ., Hitters)
# 19 Variables  (and intercept)
#            Forced in Forced out
# AtBat          FALSE      FALSE
# Hits           FALSE      FALSE
# HmRun          FALSE      FALSE
# Runs           FALSE      FALSE
# RBI            FALSE      FALSE
# Walks          FALSE      FALSE
# Years          FALSE      FALSE
# CAtBat         FALSE      FALSE
# CHits          FALSE      FALSE
# CHmRun         FALSE      FALSE
# CRuns          FALSE      FALSE
# CRBI           FALSE      FALSE
# CWalks         FALSE      FALSE
# LeagueN        FALSE      FALSE
# DivisionW      FALSE      FALSE
# PutOuts        FALSE      FALSE
# Assists        FALSE      FALSE
# Errors         FALSE      FALSE
# NewLeagueN     FALSE      FALSE

# 1 subsets of each size up to 8

# Selection Algorithm: exhaustive
#          AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI
# 1  ( 1 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*" 
# 2  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*" 
# 3  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*" 
# 4  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*" 
# 5  ( 1 ) "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*" 
# 6  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*" 
# 7  ( 1 ) " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " " 
# 8  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"   " " 
#          CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
# 1  ( 1 ) " "    " "     " "       " "     " "     " "    " "       
# 2  ( 1 ) " "    " "     " "       " "     " "     " "    " "       
# 3  ( 1 ) " "    " "     " "       "*"     " "     " "    " "       
# 4  ( 1 ) " "    " "     "*"       "*"     " "     " "    " "       
# 5  ( 1 ) " "    " "     "*"       "*"     " "     " "    " "       
# 6  ( 1 ) " "    " "     "*"       "*"     " "     " "    " "       
# 7  ( 1 ) " "    " "     "*"       "*"     " "     " "    " "       
# 8  ( 1 ) "*"    " "     "*"       "*"     " "     " "    " "       

# Note the best 2 variable model contains Hits and CRBI

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)

# Summary Data
reg.summary=summary(regfit.full)
names(reg.summary)
# [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"   

reg.summary$rsq # Rsquared
#  [1] 0.3214501 0.4252237 0.4514294 0.4754067 0.4908036 0.5087146 0.5141227 0.5285569 0.5346124 0.5404950 0.5426153
# [12] 0.5436302 0.5444570 0.5452164 0.5454692 0.5457656 0.5459518 0.5460945 0.5461159

par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables", ylab="RSS", type="l")

m.radjr2 = which.max(reg.summary$adjr2) # 11
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted Rsq", type="l")
points(m.radjr2, reg.summary$radjr2[m.radjr2], col="red", cex=2, pch=20)

cp = which.min(reg.summary$cp) # 10
plot(reg.summary$cp, xlab="Number of Variables", ylab="Gp", type="l")
points(cp, reg.summary$cp[cp], col="red", cex=2, pch=20)

bic = which.min(reg.summary$bic) # 6
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(bic, reg.summary$bic[bic], col="red", cex=2, pch=20)

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

# coef for estimates
coef(regfit.full,bic)
#  (Intercept)        AtBat         Hits        Walks         CRBI    DivisionW      PutOuts 
#   91.5117981   -1.8685892    7.6043976    3.6976468    0.6430169 -122.9515338    0.2643076 
