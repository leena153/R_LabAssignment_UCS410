#Question 1
# A pipe manufacturing organization produces different kinds of pipes. We are given
# the monthly data of the wall thickness of certain types of pipes (data is available on
#                                                                   LMS Clt-data.csv).
# The organization has an analysis to perform and one of the basic assumption of that
# analysis is that the data should be normally distributed.
# You have the following tasks to do:
# (a) Import the csv data file in R.
# (b) Validate data for correctness by counting number of rows and viewing the top
# ten rows of the dataset.
# (c) Calculate the population mean and plot the observations by making a histogram.
# (d) Mark the mean computed in last step by using the function abline.

df <- read.csv(file.choose()) #(a)
summary(df)

dim(df) #(b)
head(df,10)

m <- mean(df$Wall.Thickness) #(c)
print(m)
hist(df$Wall.Thickness)

abline(v=m,col = "red", lty = 2, lwd = 2) #(d)

#if max on mean line then it's standard normal distribution else it's normal distribution

#Question 2
# (a) Draw sufficient samples of size 10, calculate their means, and plot them in R
# by making histogram. Do you get a normal distribution.
# (b) Now repeat the same with sample size 50, 500 and 9000. Can you comment on
# what you observe.

#(a)
s10 <- c() 

for(i in 1:10){
  s10[i] <- mean(sample(df$Wall.Thickness, 10, TRUE))
}

hist(s10)

#(b)
par(mfrow = c(1,3))

s50 <- c();
for(i in 1:50){
  s50[i] <- mean(sample(df$Wall.Thickness,50,TRUE))
}

hist(s50)
abline(v=mean(s50), col = "red", lty = 2, lwd = 2)

s500 <- c();
for(i in 1:500){
  s500[i] <- mean(sample(df$Wall.Thickness,500,TRUE))
}

hist(s500)
abline(v=mean(s500), col = "blue", lty = 2, lwd = 2)

s9000 <- c();
for(i in 1:9000){
  s9000[i] <- mean(sample(df$Wall.Thickness,9000,TRUE))
}

hist(s9000)
abline(v=mean(s9000), col = "yellow", lty = 2, lwd = 2)
