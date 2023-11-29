# (1) Use the rt(n, df) function in r to investigate the t-distribution for n = 100 and df = n − 1 and plot
# the histogram for the same.

n <- 100
df <- n-1
p <- rt(n,df)
print(p)
hist(p, col = "yellow")

# (2) Use the rchisq(n, df) function in r to investigate the chi-square distribution with n = 100 and
# df = 2, 10, 25.

n <- 100
df <- c(2,10,25)
r1 <- rchisq(n,df[1])
r2 <- rchisq(n,df[2])
r3 <- rchisq(n,df[3])
par(mfrow=c(1,3))
hist(r1, col ="yellow")
hist(r2, col ="red")
hist(r3, col ="cyan")

# (3) Generate a vector of 100 values between -6 and 6. Use the dt() function in r to find the values of a
# t-distribution given a random variable x and degrees of freedom 1,4,10,30. Using these values plot
# the density function for students t-distribution with degrees of freedom 30. Also shows a comparison
# of probability density functions having different degrees of freedom (1,4,10,30).

x <- seq(-6, 6, length = 100)
d1 <- dt(x,1)
d2 <- dt(x,4)
d3 <- dt(x,10)
d4 <- dt(x,30)

plot(x,d4,col="red")
lines(x,d2,col="yellow")
lines(x,d3,col="blue")
lines(x,d1,col="green")


# (4) Write a r-code
# (i) To find the 95th percentile of the F-distribution with (10, 20) degrees of freedom.
# (ii) To calculate the area under the curve for the interval [0, 1.5] and the interval [1.5, +∞) of
# a F-curve with v1 = 10 and v2 = 20 (USE pf()).
# (iii) To calculate the quantile for a given area (= probability) under the curve for a F-curve
# with v1 = 10 and v2 = 20 that corresponds to q = 0.25, 0.5, 0.75 and 0.999. (use the qf())
# (iv) To generate 1000 random values from the F-distribution with v1 = 10 and v2 = 20 (use
# rf())and plot a histogram.

f1 <- qf(0.95,10,20) #(i)
print(f1)

p1 <- pf(1.5, 10, 20) #(ii)
print(p1)
p2 <- 1-p1 #method1
print(p2) 
p2 <- pf(1.5, 10, 20, lower.tail = FALSE) #method2
print(p2)

f5 <- qf(c(0.25,0.5,0.75), 10, 20) #(iii)
print(f5)

r1 <- rf(1000,10,20) #(iv)
hist(r1)