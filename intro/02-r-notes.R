# help() help.search() and example()
d + c(1, 2); d # adds 1, 2 down each column repeating



# Scope local and global
n = 10 # a global scope
scope <- function(x)
    n = x + 1
    return (n)

scope(n) # returns 11
n = # is unchanged 10

# default arguments
g  <- function(x, y=2, z='t') return(c(x, y, z))
g(c(7))

# matrix operations
rbind(c(1,4)),c(2,2)
m %*% c(1,1) # 5, 4
m[1,] # row 1
m[,1] # col 1

# lists
a <- list(u=2,v="abc")
# $u # list item named u = 2 at index 1
# [1] 2
# $v # list item named v, "abc" at index 1
# [1] "abc"

# data frames
d <- data.frame(list(kids=c("Jak", "Jil"), ages=c(12,10))); d
#   kids ages
# 1  Jak   12
# 2  Jil   10

# insert
b <- c(88,5,12,13)
b <- c(b[1:3],168,x[4])

# length
length(b) # returns 5

# recycling repeating a shorter vector until full
e = cbind(c(1,2,3), c(4,5,6)); e
e + c(1, 2); e # adds 1, 2 down each column repeating
#      [,1] [,2]
# [1,]    2    6
# [2,]    4    6
# [3,]    4    8

# matrices
m <- cbind(c(1, 2),c(3,4)); m # a normal matrix
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

m + 10:13; m # column wise addition
#      [,1] [,2]
# [1,]   11   15
# [2,]   13   17

# vector extraction
f <- c(1.2, 3.9, 0.4, 0.12)
f[c(1,3)] # returns 1.2 and 0.4

# vector exclusion
f[-1] # returns 3.9, 0.4, 0.12
f[-1:-2] # returns 0.4, 0.12
f[length(f)] # returns 0.12

# generators
5:8
5:1
seq(from=12, to=30, by=3)
rep(1:3,2) # 1 2 3 1 2 3
rep(c(5,12,13), each=2) # 55 12 12 13 13

# all or any TRUTH
g <- 1:10
any(g > 8)
any(g > 88)
all(g > 0)

# vector to matrix
h <- c(1:8, (1:8)^2)
matrix(h, ncol=2) # create by columns with number col = 2

# sapply to perform the same task
sapply(h, function(x) x^2)

# NA and NULL
i <- c(88, NA, 12, 28, 13)
mean(i) # returns NA
mean(i, na.rm=T) # returns 35.25

# filtering
j <- c(5, 2, -3, 8)
k <- j[j*j > 8] # if the square is greater than 8

# subsetting
l <- c(6,1:3,NA,12)
l[x>5] # 6 NA 12
subset(l, l>5) # 6 12

