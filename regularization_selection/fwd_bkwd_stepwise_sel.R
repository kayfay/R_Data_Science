# Import libraries
library(ISLR)
library(leaps)

names(Hitters)
#  [1] "AtBat"     "Hits"      "HmRun"     "Runs"      "RBI"       "Walks"    
#  [7] "Years"     "CAtBat"    "CHits"     "CHmRun"    "CRuns"     "CRBI"     
# [13] "CWalks"    "League"    "Division"  "PutOuts"   "Assists"   "Errors"   
# [19] "Salary"    "NewLeague"

# Omitting na values
Hitters=na.omit(Hitters)
dim(Hitters) # [1] 263  20

# Foward stepwise or backward stepwise selection
regfit.full=regsubsets(Salary~., data=Hitters, nvmax=19)
regfit.fwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
regfit.bwd=regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")

summary(regfit.fwd)
# Subset selection object
# Call: regsubsets.formula(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
# Selection Algorithm: forward
#           AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks LeagueN DivisionW PutOuts
# 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "     " "       " "    
# Note CRBI has the best one variable selection

summary(regfit.bwd)
# Subset selection object
# Call: regsubsets.formula(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
#           AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks LeagueN DivisionW PutOuts
# 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "    " "     " "       " "    
# 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"   " "  " "    " "     " "       " "    
# Note the best subset model selection variables from foward and backward selection differ


# Note the best subset selection for 7 variable selection from full, foward, and backward selection
coef(regfit.full, 7)
#  (Intercept)         Hits        Walks       CAtBat        CHits       CHmRun    DivisionW      PutOuts 
#   79.4509472    1.2833513    3.2274264   -0.3752350    1.4957073    1.4420538 -129.9866432    0.2366813 
coef(regfit.fwd, 7)
#  (Intercept)        AtBat         Hits        Walks         CRBI       CWalks    DivisionW      PutOuts 
#  109.7873062   -1.9588851    7.4498772    4.9131401    0.8537622   -0.3053070 -127.1223928    0.2533404 
coef(regfit.bwd, 7)
#  (Intercept)        AtBat         Hits        Walks        CRuns       CWalks    DivisionW      PutOuts 
#  105.6487488   -1.9762838    6.7574914    6.0558691    1.1293095   -0.7163346 -116.1692169    0.3028847 
