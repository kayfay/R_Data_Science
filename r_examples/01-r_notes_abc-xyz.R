package_types  <- function() {
    print("
Package - extension with code and data
Library - directory containing packages
Repository - website for packages
Source - text and code
Binary - compiled source
Base packages - part of R source
Recommended packages - included in installs
Contributed packages - user packages on CRAN
") }

package_types

# R code for linear regression

# formula for linear regression
# y = x' * BETA + epsilon, where epsilon ~ N(0, variance)
# lm() standard R function for linear regression
# example of OLS, ordinary of lease squares

# sample_BETA_OLS = (t(X) * X)^-1 * t(X) * y
# covariance matrix var_sample_BETA = variance * (t(X) * X)^-1

linmodEst <- function(x, y)
{
    ## compute QR-decomposition of x
    qx <- qr(x)

    ## compute (x'x)^(-1) x'y OLS
    coef <- solve.qr(qx,y)

    ## degrees of freedom and standard deviation of residuals
    df <- nrow(x)-ncol(x)
    sigma2 <- sum((y - x%*%coef)^2)/df

    ## compute sigma^2 * (x'x)^-1
    vcov <- sigma2 * chol2inv(qx$qr)
    colnames(vcov) <- rownames(vcov) <- colnames(x)

    list(coefficients = coef,
         vcov = vcov,
         sigma = sqrt(sigma2),
         df = df)
}

data(cats, package="MASS")
print("___linmodEst___"); linmodEst(cbind(1, cats$Bwt), cats$Hwt)
print("___lm1___"); lm1 <- lm(Hwt~Bwt, data=cats) ; lm1
print("___vcov___"); vcov(lm1)

# Object oriented programming in R S3 and S4 types

# The S3 system

# Classes & Methods for linear regression

# S formulas

# R packages

# Structure of a package

# Starting a package for linear regression

# The package DESCRIPTION file

# R doc files

# Data in Packages
################################
# parallel min and max
print("parrallel vector x and y")
x <- c(10.4, 5.6, 3.1, 6.4, 21.7); x
y <- c(10.2, 5.3, 3.05, 6.2, 21.35); y
pmax(x,y)
pmin(x,y)
print("parrallel vector x and y")

# generating regular sequences
print(" generating regular sequences
  n = 10 1:n-1")
n=10 ; 1:n-1;
print(" 1:(n-1)")
1:(n-1)
print(" 30:1 generates backwards sequences")
print(" seq() is the name form of using ':' for sequences")
print("seq(from=30, to=1")
30:1
print(" seq(length=.5, from=5, by=.2)-> s3")
seq(.5, 5, by=.2)-> s3

print(" rep(x,times=5)")
rep(x,times=5)
print(" rep(x,each=5)")
rep(x,each=5)

# logical vectors
print(" logical vectors, TRUE or FALSE, T or F, 1 or 0")
print(" <, <=, >=, ==, &, |, !")
print("tmp <- x > 13")
tmp <- x > 13 ;tmp

print(" missing values")
print("
z <- c(1:3,NA); ind <- is.na(z)
 x == NA & is.na(x)
")
z <- c(1:3,NA); z
ind <- is.na(z); ind
print("0/0  Inf - Inf ")

print(" character vectors")
print('paste("Hello ", z, " World")')
print('paste(c("X", "Y"), 1:10, sep="")')
print('c("X1", "Y2", "X3", "Y4", "X5", "Y6", "X7", "Y8", "Y9", "Y10")')
labels <- paste(c("X", "Y"), 1:10, sep=""); labels

print(" index vectors modify subsets of data sets y <- x[!is.na(x)] ")
y <- x[!is.na(x)] ; y
(x+1)[(!is.na(x)&x>0)] -> z; z

print(" vector of positive integral quantities x[1:10]")
x[1:10]
print(" positive integral quantitie vector  c('x','y')[rep(c(1,2,2,1), times=4)]")
c('x','y')[rep(c(1,2,2,1), times=4)]

print("exclude values with negative integral quantities: x[-(1:5)]")
y <- x[-(1:5)]; y

print("a vector of character strings")
print('fruit <- c(5, 10, 1, 20)  names(fruit) <- c("orange", "bananna", "apple", "peach") lunch <- fruit[c("apple", "orange")] ')
fruit <- c(5, 10, 1, 20)
names(fruit) <- c("orange", "bananna", "apple", "peach"); names(fruit)
lunch <- fruit[c("apple", "orange")]; lunch

print(' A vector to remove NA or NaN not a number
x <- c(10.4, 5.6, 3.1, 6.4, 21.7); x
y <- c(10.2, 5.3, 3.05, 6.2, 21.35); y
x[is.na(x)] <- 0
')

x <- c(10.4, 5.6, 3.1, 6.4, 21.7); x
y <- c(10.2, 5.3, 3.05, 6.2, 21.35); y
x[is.na(x)] <- 0

print("y[y < 0] <- -y[y < 0] is y <- abs(y)")
y[y < 0] <- -y[y < 0]; y

