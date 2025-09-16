#All code and outputs to be sent to oliver.shipley@stonybrook.edu by end of class.
# MAR504 –Workshop 2 – Probability Theory and Distributions
# Part A
# 1. How many different ways can the first three places be decided in a race with four runners ? Hint# Factorial

# 4! = 4*3*2*1= 24 ways

# 2. How many different arrangements of the letters MISSISSIPPI ? Hint# Factorial

# 11! = 39916800

# 3. Find the probability for a binomial experiment. There are 20 trials, the probability of a success is 0.01, and you want to find the probability of 1 success (P(x = 1))

n = 20
p = 0.01
k = 1
prob <- dbinom(k, size = n, prob = p)
print(prob * 100)
# 16.52 % probability.

# 4. For the same trial, find the probability for at most 6 successes.

prob <- pbinom(6, 20, 0.01)
print(prob * 100)
# 100 % probability.

# Part B
# 1. Randomly sample 100 times from a normal distribution with a mean of 50 and a standard deviation of 10. Save the new data (this is called a vector) as an object.

my_data <- rnorm(n = 100, mean = 50, sd = 10)

# 2. Make a histogram of the resampled data and specify the use of 8 breaks on the x -
#   axis.

hist(my_data,
     breaks = 8,
     main = "Histogram of resampled data",
     xlab = "values")

# 3. Generate associated descriptive statistics. Does the resampled data match the original mean and SD specified in part 1 ?

summary(my_data)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 16.43   42.61   47.81   48.79   55.36   76.75
sd(my_data)
# [1] 10.68993
# The data's mean and sd do not match the original, it is more sensitive.

# 4. Plot the probability density function of the vector and save this file.

dens <- density(my_data)
plot(dens,
     main = "PDF of my_data",
     xlab = "values",
     ylab = "density")

# 4. Plot the cumulative distribution of the vector and save this file.

ecdf_data <- ecdf(my_data)
plot(ecdf_data,
     main = "CDF of my_data",
     xlab = "values",
     ylab = "cumulative distribution")

# 5. Describe the difference between the probability density function and cumulative distribution function.

#Cumulative distribution shows the proportion of data <= each value while the probability densition function shows you the amount of times the data is within a certain probability range.

# Part C
# 1. Read in the penguin data file ‘penguin_adapted.csv’.
install.packages("readxl")
install.packages("readr")
library(readxl)
library(readr)
penguins <- read_excel("/Users/reneechabot-mehlin/Downloads/penguins.xlsx")

# 2. Assess whether each of the four associated variables conform to a normal distribution. Report these results for each variable along with a histogram.

penguins$bill_length_mm <- as.numeric(penguins$bill_length_mm)
shapiro.test(penguins$bill_length_mm) # W = 0.97485, p-value = 1.12e-05
hist(penguins$bill_length_mm, xlab = "Bill Length (mm)")

penguins$bill_depth_mm <- as.numeric(penguins$bill_depth_mm)
shapiro.test(penguins$bill_depth_mm) # W = 0.97258, p-value = 4.419e-06
hist(penguins$bill_depth_mm, xlab = "Bill Depth (mm)")

penguins$flipper_length_mm <- as.numeric(penguins$flipper_length_mm)
shapiro.test(penguins$flipper_length_mm) #W = 0.95155, p-value = 3.54e-09
hist(penguins$flipper_length_mm, xlab = "Flipper Length (mm)")

penguins$body_mass_g <- as.numeric(penguins$body_mass_g)
shapiro.test(penguins$body_mass_g) #W = 0.95921, p-value = 3.679e-08
hist(penguins$body_mass_g, xlab = "Body Mass (g)")


# 3. For variables that conform to a normal distribution, generated a probability density function and a cumulative distribution function. Save each respective plot.

#None of them conform as their p-values are not >= 0.05.

# 4. Calculate densities at three specific points for two of these variables and report these values.

dnorm(
  c(30, 35, 40),
  mean(penguins$bill_length_mm, na.rm = T),
  sd(penguins$bill_length_mm, na.rm = T)
) # [1] 0.002829778 0.019224315 0.056453984
dnorm(
  c(14, 18, 22),
  mean(penguins$bill_depth_mm, na.rm = T),
  sd(penguins$bill_depth_mm, na.rm = T)
) # [1] 0.056555937 0.184191332 0.009914059