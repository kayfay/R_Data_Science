Name <- c("Bob", "Bill", "Betty")
Test1 <- c(80, 95, 92)
Test2 <- c(40, 87, 90)
grades <- cbind(Name, Test1, Test2) ; grades
#    Name    Test1 Test2
# [1,] "Bob"   "80"  "40"
# [2,] "Bill"  "95"  "87"
# [3,] "Betty" "92"  "90"

grades.df <- data.frame(Name, Test1, Test2) ; grades.df
#    Name Test1 Test2
# 1   Bob    80    40
# 2  Bill    95    87
# 3 Betty    92    90

lapply(grades.df[,2:3], mean)
# $Test1
# [1] 89
#
# $Test2
# [1] 72.33333

grades.df$Name <- c(3, 4, 5); grades.df
#   Name Test1 Test2
# 1    3    80    40
# 2    4    95    87
# 3    5    92    90

as.matrix(grades.df)%*%c(1,2,3)
#      [,1]
# [1,]  283
# [2,]  455
# [3,]  459

mean(as.matrix(grades.df)%*%c(1,2,3))
# [1] 399


Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Berine")
ABC_Poll_results <- c(4, 62, 51, 21, 2, 14, 15)
NCB_Poll_results <- c(12, 75, 43, 19, 1, 21, 19)
M <- cbind(Name, ABC_Poll_results, NCB_Poll_results); M
#      Name      ABC_Poll_results NCB_Poll_results
# [1,] "Jeb"     "4"              "12"
# [2,] "Donald"  "62"             "75"
# [3,] "Ted"     "51"             "43"
# [4,] "Marco"   "21"             "19"
# [5,] "Carly"   "2"              "1"
# [6,] "Hillary" "14"             "21"
# [7,] "Berine"  "15"             "19"


M.df <- data.frame(Name, ABC_Poll_results, NCB_Poll_results)
M.df[,2:3]
#   ABC_Poll_results NCB_Poll_results
# 1                4               12
# 2               62               75
# 3               51               43
# 4               21               19
# 5                2                1
# 6               14               21
# 7               15               19

lapply(M.df[,2:3], mean)
# $ABC_Poll_results
# [1] 24.14286
#
# $NCB_Poll_results
# [1] 27.14286


as.matrix(M.df[,2:3])%*%c(1:2)
#      [,1]
# [1,]   28
# [2,]  212
# [3,]  137
# [4,]   59
# [5,]    4
# [6,]   56
# [7,]   53
mean(as.matrix(M.df[,2:3])%*%c(1:2))
# [1] 78.42857


