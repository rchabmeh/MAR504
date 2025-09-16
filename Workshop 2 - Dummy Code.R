#Workshop two code

#Part A

#Question 1
factorial(4)

factorial(4)/factorial(4-3)

#Question 2

result <- factorial(11) / (factorial(1) * factorial(4) * factorial(4) * factorial(2))
result

#Total of 11 letters 
#Letter M appears 1 time, I appears 4 times, S appears 4 times, P appears 2 times. 

#Question 3

dbinom(1,20,0.01)

#

pbinom(6,20, 0.01)

#Part B

#Question 1

data1 <- rnorm(100, 50, 10)

#Question 2
pdf <- dnorm(data1, mean(data1), sd(data1))
plot(data1, pdf, main="Normal Distribution PDF", xlab="x", ylab="Density")

#Question 3
cdf <- ecdf(data1)
plot(cdf, main = "CDF Graph", xlab = "x", ylab = "Probability")


data1 <- read.csv('penguins_adapted.csv', header=T)

#Bill length

pdf_billlength <- dnorm(data1$bill_length_mm, mean(data1$bill_length_mm), sd(data1$bill_length_mm))
plot(data1$bill_length_mm, pdf_billlength, main="Normal Distribution PDF", xlab="x", ylab="Density")

cdf_billlength <- ecdf(data1$bill_length_mm)
plot(cdf_billlength, main = "CDF Graph", xlab = "x", ylab = "Probability")

#Question 4

dnorm(c(30,35,40), mean(data1$bill_length_mm), sd(data1$bill_length_mm)) # Density at x=70 for a normal distribution with mean=60 and sd=5
