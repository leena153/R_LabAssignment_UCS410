# Question1
# (a) Suppose there is a chest of coins with 20 gold, 30 silver and 50 bronze coins.
# You randomly draw 10 coins from this chest. Write an R code which will give us the
# sample space for this experiment. (use of sample(): an in-built function in R)
sample(c(rep("Gold",20),rep("Silver",30),rep("bronze",50)),10)

# (b) In a surgical procedure, the chances of success and failure are 90% and 10%
# respectively. Generate a sample space for the next 10 surgical procedures performed.
# (use of prob(): an in-built function in R)
sample(c("s","f"),10, replace = TRUE, prob = c(0.9,0.1))


# Question 2
# A room has n people, and each has an equal chance of being born on any of the 365
# days of the year. (For simplicity, we’ll ignore leap years). What is the probability
# that two people in the room have the same birthday?
# (a) Use an R simulation to estimate this for various n.
# (b) Find the smallest value of n for which the probability of a match is greater than .5.

birthdayparadox <- function(n){ 
  return(1-(364/365)**(n*(n-1)/2)) #(364/365)^(nC2)
}
cat(birthdayparadox(23)) #(a)

smallest_n <- function(){ 
  p=1
  for(i in 1:365){
    p = p*(365-i)/365
    if(1-p>=0.5){
      return(i+1)
    }
  }
}
cat(smallest_n()) #(b)


# Question 3
# Write an R function for computing conditional probability. Call this function to do
# the following problem:
# suppose the probability of the weather being cloudy is 40%. Also suppose the prob-
# ability of rain on a given day is 20% and that the probability of clouds on a rainy day
# is 85%. If it’s cloudy outside on a given day, what is the probability that it will rain
# that day?

bayes_prc<-function(pr,pcr,pc){
  cat(pr*pcr/pc)
}
bayes_prc(0.2,0.85,0.4)


# Question 4
# The iris dataset is a built-in dataset in R that contains measurements on 4 different
# attributes (in centimeters) for 150 flowers from 3 different species. Load this dataset
# and do the following:
# (a) Print first few rows of this dataset.
# (b) Find the structure of this dataset.
# (c) Find the range of the data regarding the sepal length of flowers.
# (d) Find the mean of the sepal length.
# (e) Find the median of the sepal length.
# (f) Find the first and the third quartiles and hence the interquartile range.
# (g) Find the standard deviation and variance.
# (h) Try doing the above exercises for sepal.width, petal.length and petal.width.
# (i) Use the built-in function summary on the dataset Iris.

dataset<-iris
head(dataset,10) #a
str(dataset) #b
range(dataset$Sepal.Length) #c
mean(dataset$Sepal.Length) #d
median(dataset$Sepal.Length) #e
quantile(dataset$Sepal.Length,0.25) #f
quantile(dataset$Sepal.Length,0.75)
IQR(dataset$Sepal.Length)
variance <- var(dataset$Sepal.Length) #g
sd <- sqrt(variance)
cat(variance," ",sd)
summary(dataset) #i


# Question 5
# R does not have a standard in-built function to calculate mode. So we create a user
# function to calculate mode of a data set in R. This function takes the vector as input
# and gives the mode value as output.

mode_vector <- function(v){
  uniq<-unique(v)
  uniq[which.max(tabulate(match(v,uniq)))]
}
print(mode_vector(c(1,7,3,4,5,6,7,1,2,4,6,4,3,4,3,4,4,2,2,2)))


