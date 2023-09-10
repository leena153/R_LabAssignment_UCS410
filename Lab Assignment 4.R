# Question 1
# The probability distribution of X, the number of imperfections per 10 meters of a
# synthetic fabric in continuous rolls of uniform width, is given as
# x 0 1 2 3 4
# p(x) 0.41 0.37 0.16 0.05 0.01
# Find the average number of imperfections per 10 meters of this fabric.
# (Try functions sum( ), weighted.mean( ), c(a %*% b) to find expected value/mean.

x<-c(0,1,2,3,4)
px<-c(0.41,0.37,0.16,0.05,0.01)
sum(x*px)
weighted.mean(x,px)
c(x%*%px)


# Question 2
# The time T, in days, required for the completion of a contracted project is a random
# variable with probability density function f(t) = 0.1 e(-0.1t) for t > 0 and 0 otherwise.
# Find the expected value of T.
# Use function integrate( ) to find the expected value of continuous random variable T.

ft<-function(t){
  a<-t*0.1*exp(-0.1*t)
}
integrate(ft,0,Inf)


# Question 3
# A bookstore purchases three copies of a book at $6.00 each and sells them for $12.00
# each. Unsold copies are returned for $2.00 each. Let X = {number of copies sold} and
# Y = {net revenue}. If the probability mass function of X is
# x 0 1 2 3
# p(x) 0.1 0.2 0.2 0.5
# Find the expected value of Y.

x <- c(0,1,2,3)
px <- c(0.1,0.2,0.2,0.5)
# 12*x-18+(3-x)*2 = 10x - 12
sum((10*x-12)*px)


# Question 4
# Find the first and second moments about the origin of the random variable X with
# probability density function f(x) = 0.5e^-|x|,1 < x < 10 and 0 otherwise. Further use
# the results to find Mean and Variance.
# (kth moment = E(X^k), Mean = first moment and Variance = second moment – Mean2

f1<-function(x){
  a<-0.5*x*exp(-abs(x))
}
f2<-function(x){
  a<-0.5*x*x*exp(-abs(x))
}
mean_expec <- integrate(f1,1,10)
a <- integrate(f2,1,10)
variance_expec <- a$value - mean_expec$value**2
print(c(mean_expec$value, variance_expec))

# Question 5
# Let X be a geometric random variable with probability distribution
# f(x)=3/4*(1/4)^x-1, x = 1,2,3..
# Write a function to find the probability distribution of the random variable Y = X^2 and
# find probability of Y for X = 3. Further, use it to find the expected value and variance of
# Y for X = 1,2,3,4,5.

x<-c(1:5)
prob_x <-(3/4)*((1/4)**(x-1))
prob_y <- prob_x
print(prob_y[3])
y<-x*x
Ey = sum(y*prob_y)
vary = sum(y*y*prob_y) - Ey**2
cat(Ey)
cat(vary)





