# Question 1
# Consider that X is the time (in minutes) that a person has to wait in order to take a flight.
# If each flight takes off each hour X ~ U(0, 60). Find the probability that
# (a) waiting time is more than 45 minutes, and
# (b) waiting time lies between 20 and 30 minutes.

a <- punif(45,0, 60, lower.tail = FALSE) #(a)
print(a)

b <- punif(30, 0, 60) - punif(20, 0, 60) #(b)
print(b)

# Question 2
# The time (in hours) required to repair a machine is an exponential distributed random
# variable with parameter λ = 1/2.
# (a) Find the value of density function at x = 3.
# (b) Plot the graph of exponential probability distribution for 0 ≤ x ≤ 5.
# (c) Find the probability that a repair time takes at most 3 hours.
# (d) Plot the graph of cumulative exponential probabilities for 0 ≤ x ≤ 5.
# (e) Simulate 1000 exponential distributed random numbers with λ = 1⁄2 and plot the
# simulated data.

lambda <- 1/2

a <- dexp(3, lambda) #(a)
print(a)

x <- seq(0, 5, 0.001) #(b)
plot(x, dexp(x,lambda), type = "l", lwd = 2, col = "red")

c <- pexp(3, lambda) #(c)
print(c)

plot(x, pexp(x,lambda), type = "l", lwd = 2, col = "red") #(d)

x <- rexp(1000, lambda) #(e)

par(mfrow = c(2, 1))

hist(x)
plot(density(x))


# Question 3
# The lifetime of certain equipment is described by a random variable X that follows
# Gamma distribution with parameters α = 2 and β = 1/3.
# (a) Find the probability that the lifetime of equipment is at least 1 unit of time.
# (b) What is the value of c, if P(X ≤ c) ≥ 0.70? (Hint: try quantile function qgamma())
# alpha - shape, #beta - scale
# pgamma(X,shape,size)

alpha <- 2
beta <- 1/3

a <- pgamma(1, shape = alpha, scale = beta, lower.tail = FALSE) #(a)
print(a)

b <- qgamma(0.70, shape = alpha, scale = 1/3) #(b)
print(b)
