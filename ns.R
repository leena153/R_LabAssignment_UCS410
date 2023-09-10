# Q1(a)
v1 = c(rep("Gold", 20), rep("Silver", 30), rep("Bronze", 50));
sample(v1, size=10);

# Q1(b)
sample(c("s", "f"), size = 10, replace = T, prob = c(0.9,0.1));

# Q2
birthday_paradox <- function(n){
  for(i in 1:n-1){
    prob = 1 - ((1/365)^(fact(i)/((fact(i-2))*2)));
    cat(prob);
    if(prob>=0.5){
      cat(i);
      break;
    }
  }
}
fact <- function(i){
  m = 1;
  if(i==1){
    return(1);
  }
  for(j in 1:i){
    m=m*j;
  }
  return(m);
}
birthday_paradox(100)

birthday_prob <- function(n, days) {
  prod <- 1
  for (i in 1:n) {
    prod <- prod * (days - i) / days
  }
  return(1 - (prod))
}

print(birthday_prob(23,365))

# Q3
bayes <- function(pc, pr, pcr){
  prc = pr*pcr/pc;
  cat(prc);
}
bayes(0.4,0.2,0.85)

# Q4
dset = iris;
head(dset,5);
str(dset) #structure
a=dset$Sepal.Length #values of the specific column
range(a)
mean(a)
median(dset$Petal.Length)
quantile(a,0.25)
quantile(a,0.75)
IQR(a)
sd(a)
var(a)
summary(dset)
lapply(dset[,1:3],sd)

# Q5
mode_vector <- function(v){
  uniq<-unique(v);
  uniq[which.max(tabulate(match(v,uniq)))]
}
print(mode_vector(c(1,2,3,4,5,6,7,1,2,4,6,4,3,4,3,4,4)))