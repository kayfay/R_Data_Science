print("arithmetic, relational, logical, assignment, and miscellaneous operators")

v <- c(4, 10, 3, 12, 8, 15)
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow=2)
n <- matrix(c(2, 4, 6, 8, 10, 12), nrow=2)
u <- matrix(c(3, 9, 12, 15, 18, 21), nrow=3)

cat("\n")
print(c(v, m))
print("vector + matrix")
v + m

cat("\n")
print("vector - matrix")
print(c(v, n))
v - n

cat("\n")
print("vector * matrix, %*% with its transpose")
print(c(v, u))
v * u

cat("\n")
print("vector / transpose matrix")
print(c(v, t(u)))
v / t(u)

cat("\n")
print("vector %% vector")
print(c(v, v))
v %% v

cat("\n")
print("vector %/% transpose matrix")
print(c(v, t(u)))
v %/% t(u)
cat("\n")
print("vector v^2")
print(v)
v^2

cat("\n")
print("logical vectors: m > n")
print("m"); m; print("n"); n

cat("\n")
v > n

print("logical vectors: m > n, and <, ==, <=, >=, !=")
print("logical operators with element wise &, |, !")
print("logical operators with the first element &&, ||")


cat("\n")
print("sequences with :")
print("1:9")
1:9

