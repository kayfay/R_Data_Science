if(FALSE) {"This is a demo for multi-line comments and it should be put inside
double quotes" }

cat("\n")
v <- TRUE
print(c("TRUE", class(v)))

v <- 23.5
print(c("23.5", class(v)))

v <- 2L
print(c("2L", class(v)))

v <- 2+5i
print(c("2+5i", class(v)))

v <- "TRUE"
print(c('"TRUE"', class(v)))

v <- charToRaw("Hello")
print(c("Raw Hello", class(v)))

cat("\n")
print("The most basic data type in R is a vector")
print("in linear algebra, a column vector or vector")
apple <- c("red", "green", "yellow")
print(c("apple vector:", apple))
print(class(apple))

cat("\n")
print("Lists contain differing elements")
print("are objects of vectors")
list1 <- list(c(2,5,3),
              21.3,
              sine=sin)
print("list1 <- list(c(2,5,3), 21.3, sine=sin)")
cat("\n")
print(list1)


cat("\n")
print("m by n Matrices")
print(("M <- matrix(c('a','a','b','c','b','a'), nrow=2, ncol=3, byrow=TRUE"))
print("Sets the vector by 2 rows and 3 column, enumerating by row")
M <- matrix(c('a','a','b','c','b','a'), nrow=2, ncol=3, byrow=TRUE)
cat("\n")
print(M)

cat("\n")
print("Multi-dimensional arrays")
print('a <- array(c(green, yellow), dim=c(3,3,2))')
a <- array(c("green", "yellow"), dim=c(3,3,2))
cat("\n")
print(a)


cat("\n")
print("Factors are vectors with distinct values of levels")
print("apple_colors <- c('green', 'green', 'yellow', 'red', 'red', 'red', 'green') ")
print("factor_apple <- factor(apple_colors) ")
print("The levels in this case are colors and nlevels are 3")
cat("\n")
apple_colors <- c('green', 'green', 'yellow', 'red', 'red', 'red', 'green')
factor_apple <- factor(apple_colors)
print(factor_apple)
print(nlevels(factor_apple))

cat("\n")
print('data frames are tabular data objects')
cat("BMI <- data.frame(
                \ngender = c(Male, Male, Female),
                \nheight = c(152, 171.5, 165),
                \nweight = c(81, 93, 78),
                \nAge = c(42, 38, 26)
                \n)")

BMI <- data.frame(
                  gender = c("Male", "Male", "Female"),
                  height = c(152, 171.5, 165),
                  weight = c(81, 93, 78),
                  Age = c(42, 38, 26)
                  )

print(BMI)
