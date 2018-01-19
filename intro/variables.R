if(FALSE) {"This is a demo for multi-line comments and it should be put inside
double quotes" }

v <- TRUE
var.1 <- 1
var.2 <- 1L
var.3 <- 4+3i
my_var <- "string"
my_new_var <- c('v', 'ec', 'tor')
var.name <- list('a', name='variable')
var_x <- array(c(1, 2), dim=c(3,3,2))

print(print(ls()))
print(ls())

cat("\n")
print("rm(var.3)")
rm(var.3)
cat("\n")

print("rm deletes a specfic object")
cat("\n")

print("print(ls())")
print(ls())
cat("\n")

print("by using rm(list=ls()) we make a list of objects")
rm(list=ls())

print("print(ls()), returns a 0 value")
print(ls())

