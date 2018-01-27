
n <- c(0, 2, 4.3, 7, -10) # numeric vector
class(n)
c <- c("one", "two", "three") # character vector
class(c)
l <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE) # logical vector
class(l)

# a matrix
x <- matrix(1:20, nrow=5, ncol=4,
    byrow=FALSE, dimnames=list(c(1, 2, 3, 4, 5), c(1, 2, 3, 4))); x

#   1  2  3  4
# 1 1  6 11 16
# 2 2  7 12 17
# 3 3  8 13 18
# 4 4  9 14 19
# 5 5 10 15 20

# arrays are multidimensional matrices
array(data=1:8, dim=c(2, 4, 2))
# , , 1
#
#      [,1] [,2] [,3] [,4]
# [1,]    1    3    5    7
# [2,]    2    4    6    8
#
# , , 2
#
#      [,1] [,2] [,3] [,4]
# [1,]    1    3    5    7
# [2,]    2    4    6    8


letters; dim(as.array(letters))
# [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"
# [1] 26
