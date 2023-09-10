# Q1
x <- c(0,1,2,3,4);
px <- c(0.41, 0.37, 0.16, 0.05, 0.01);
cat(c(x %*% px));
cat(sum(x*px));
cat(weighted.mean(x,px));

# Q2
f1<-function(t){
  a <- t * 0.1 * exp((-0.1)*t);
}
E <- integrate(f1,0,Inf);
print(E);

# Q4
f1<-function(x){
  a<- 0.5*exp(-1*abs(x))
  return (a)
}
convolution<-function(x,n){
  b<- x^n*f1
}

kmom<-function(n){
  E<-integrate(convolution(n),1,10)
}
kmom(1)

#0.997




f3<-function(x){
  return(x*0.5*exp(-abs(x)))
}
f4<-function(x){
  return(x*x*0.5*exp(-abs(x)))
}
m2<-integrate(f4,1,10)
print(m2)
m1<-integrate(f3,1,10)
print(m1)
var1<-m2$value-m1$value**2
print(var1)

