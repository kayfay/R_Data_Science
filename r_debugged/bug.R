# function decleration
tukey.outlier <- function(x) {
    q1 <- unname(quantile(x)["25%"])
    q3 <- unname(quantile(x)["75%"])
    ex.IQR <- (q3 - q1) * 1.5
    l.f <- q1 - ex.IQR
    u.f <- q3 + ex.IQR
    return(x > l.f & x < u.f)
}

tukey_multiple <- function(x) {

     # initialize an logical array with the dimensions
     # from the passed in parameter
     outliers <- array(TRUE,dim=dim(x))

     # for loop iterating based on number
     # columns of the matrix
     for (j in 1:ncol(x))
         {
             # based on the current column in [row, column]
             # overwrite the column with a column of logicals
             # evaluated with a tukey.outlier function
             outliers[,j] <- outliers[,j] & tukey.outlier(x)[,j]
         }

     # initialize a vector based n the number of rows in
     # the passed in parameter
     outlier.vec <- vector(length=nrow(x))

     for (i in 1:nrow(x))
         {
             # are all values in the vector true
             # i.e. locate the row in x that has
             # an outlier
             outlier.vec[i] <- all(outliers[i,])
         }

     return(outlier.vec)
}

x <- 1:12 ; dim(x) <- c(3,4)
# create an outlier
x[2,2] <- 1000

tukey_multiple(x)
