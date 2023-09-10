#Question 1
# Create a vector c = [5,10,15,20,25,30] and write a program which returns the max-
# imum and minimum of this vector.

c <- c(5,10,15,20,25,30)
max(c)
min(c)


# Question 2
# Write a program in R to find factorial of a number by taking input from user. Please
# print error message if the input number is negative.

fact <- function(num) {
  if(num == 1){
    return(1)
  }else if(num==0){
    return(0)
  }else if(num<0){
    return("Error")
  }else{
    result<-num*fact(num-1)
    return(result)
  }
}
print(fact(as.integer(readline("Enter your number"))))


#Question 3
# Write a program to write first n terms of a Fibonacci sequence. You may take n as an
# input from the user.

fibonacci<-function(n){
  if(n==1){
    print(0)
  }else if(n==2){
    print(c(0,1))
  }else{
    a = 0
    b = 1
    print(a)
    print(b)
    for(i in 3:n){
      c <- a+b
      print(c)
      a<-b
      b<-c
    }
  }
}
fibonacci(as.integer(readline("Enter the number of terms")))


#Question 4
# Write an R program to make a simple calculator which can add, subtract, multiply
# and divide.

add <- function(x, y) {
  return(x + y)
}
subtract <- function(x, y) {
  return(x - y)
}
multiply <- function(x, y) {
  return(x * y)
}
divide <- function(x, y) {
  if (y != 0) {
    return(x / y)
  } else {
    return("Cannot divide by zero")
  }
}
calculator <- function() {
  cat("Select operation:\n")
  cat("1. Add\n")
  cat("2. Subtract\n")
  cat("3. Multiply\n")
  cat("4. Divide\n")
  
  choice <- as.integer(readline("Enter choice (1/2/3/4): "))
  
  if (choice >= 1 && choice <= 4) {
    num1 <- as.numeric(readline("Enter first number: "))
    num2 <- as.numeric(readline("Enter second number: "))
    result <- switch(choice,
                     "1" = add(num1, num2),
                     "2" = subtract(num1, num2),
                     "3" = multiply(num1, num2),
                     "4" = divide(num1, num2))
    
    cat("Result: ", result, "\n")
  } else {
    cat("Invalid choice\n")
  }
}
calculator()
