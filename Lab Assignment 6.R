# Question 1
# The joint probability density of two random variables X and Y is
# 
# f(x, y) = 2(2x + 3y)/5; 0 ≤ x, y ≤ 1
# 0; elsewhere
# 
# Then write a R-code to
# (i) check that it is a joint density function or not? (Use integral2())
# (ii) find marginal distribution g(x) at x = 1.
# (iii) find the marginal distribution h(y) at y = 0.
# (iv) find the expected value of g(x, y) = xy.

f <- function(x,y){
  2*(2*x+3*y)/5
}

p <- integral2(f,0,1,0,1)

print(p$Q) #p$Q; //$value for single integral and $Q for double integral

if(p$Q == '1'){ #(i)
  print("It is a joint density function")
}else{
  print("It is NOT a joint density function")
}

###########

f1 <- function(y){ #(ii)
  f(1,y)
}

p <- integral(f1,0,1)

print(paste("Marginal Distribution of g(x=1) is ",p)) #$value not needed when using pracma package

###########

f2 <- function(x){ #(iii)
  f(x,0)
}

p <- integral(f2,0,1)


print(paste("Marginal Distribution of h(y=0) is ",p))

###########

gf <- function(x,y){ #(iv)
  x*y*f(x,y)
}

gfxy <- integral2(gf,0,1,0,1)

print(gfxy$Q)

# Question 2
# The joint probability mass function of two random variables X and Y is
# f(x, y) = {(x + y)/30; x = 0, 1, 2, 3; y = 0, 1, 2}
# 
# Then write a R-code to
# (i) display the joint mass function in rectangular (matrix) form.
# (ii) check that it is joint mass function or not? (use: Sum())
# (iii) find the marginal distribution g(x) for x = 0, 1, 2, 3. (Use:apply())
# (iv) find the marginal distribution h(y) for y = 0, 1, 2. (Use:apply())
# (v) find the conditional probability at x = 0 given y = 1.
# (vi) find E(x), E(y), E(xy), V ar(x), V ar(y), Cov(x, y) and its correlation coefficient.

f <- function(x,y){ #(i)
  (x+y)/30
}

x <- c(0:3)
y <- c(0:2)

M <- outer(x, y, f) #method1
print(M)

M <- matrix(c(f(0,y),f(1,y),f(2,y), f(3,y)),nrow = 4, ncol = 3, byrow = TRUE) #method 2
print(M)

##########

if(sum(M)==1){ #(ii)
  print("f(x,y) is a joint mass function")
}else{
  print("f(x,y) is not a joint mass function")
}

##########

gx <- apply(M, 1, sum)#middle value: 1-row wise,2-column wise, third value: function(usually sum and product) #(iii)
print(gx)

##########

hy <- apply(M, 2, sum) #(iv)
print(hy)

##########

cp <- M[1,2]/hy[2] #(v)
print(cp)

##########
Ex <- sum(x*gx) #(vi)
print(Ex)

Ey <- sum(y*hy)
print(Ey)

#Exy
f1 <- function(x,y){ #method1
  x*y*f(x,y)
}
M1 <- outer(x,y,f1)
Exy <- sum(M1)
print(Exy)

f1<-function(x,y) { #method 2
  x*y*f(x,y)
}
M1<-matrix(c(f1(0,0:2),f1(1,0:2),f1(2,0:2),f1(3,0:2)),nrow=4,ncol=3,byrow=TRUE)
E_xy <- sum(M1)
print(Exy)

#var(x)
Exx <- sum(x*x*gx)
varx <- Exx - Ex*Ex
print(varx)

#var(y)
Eyy <- sum(y*y*hy)
vary <- Eyy - Ey*Ey
print(vary)

#cov(xy)
covxy <- Exy - Ex*Ey
print(covxy)

#cor(xy)
cor <- covxy/sqrt(varx*vary)
print(cor)