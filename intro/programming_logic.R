boolean_expression = 1

# if statements control logical flow
if(88/4 != 22){
    print("something is very wrong")
}

if(boolean_expression) {
    print("this is right")
} else {
    print("this isn't")
}

# switch(expression, case1, case2, case3...)

choice = 2
x <- switch(choice, "case1", "case2", "case3"); print(x)
# case2

# repeat {
#     commands
#     if(condition){
#         break
#     }
# }

v <- c("Hello", "Loop")
cnt <- 2

repeat{
       print(v)
       cnt <- cnt + 1
       if(cnt > 5) {
         break
    }
}

# while(test_expression){
#     statement
# }

v <- c("Hello", "while loop")
cnt <- 2

while(cnt < 7) {
    print(v)
    cnt = cnt + 1
}

# for (value in vector) {
#     statements
# }

v <- LETTERS[1:4]
for(i in v) {
    print(i)
}

# # This is a user-defined function
# function_name <- function(arg_1, arg_2, ...) {
#     Function body
# }
# note a function name, "function"
# note arguments or default arguments arg_3 = a
# note a function body
# note a return value

# note that built in functions also exist
print(seq(32, 44))
print(mean(25:82))
print(sum(41:68))

new.function <- function(a = 5)
    for(i in 1:a){
        b <- i^2
        print(b)
    }

new.function(15)


