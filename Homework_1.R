## Homework #1
# Part B – Probability Theory and Distributions (20 Points)
#
# 1. Build a histogram with some simulated data (i.e., data you have used R to generate for
# you) to illustrate a normal distribution. Run a normality test to confirm this and report the
# associated statistics. Justify your choice of normality test. (6)


gen_r_data <- rnorm(n = 100, mean = 15, sd = 0.5)
shapiro.test(gen_r_data)

# Shapiro-Wilk normality test
# data:  gen_r_data
# W = 0.98894, p-value = 0.5799
# This is a normal distribution as the p-value > 0.5.


hist(
  gen_r_data,
  xlab = "Generated Data",
  main = "Generated R Data Histogram",
  sub = "MAR 504 Part B",
  col = "blue"
)

# 2. Plot the corresponding probability density function and cumulative distribution function.
# (4)

dens_gen_data <- density(gen_r_data)

plot(dens_gen_data,
xlab = "Generated Data",
main = "Generated R Data Probability Density",
sub = "MAR 504 Part B",
col = "blue"
)

cumulative_gen_data <- ecdf(gen_r_data)
plot(cumulative_gen_data,
     xlab = "Generated Data",
     main = "Generated R Data Cumulative Density",
     sub = "MAR 504 Part B",
     col = "blue"
)
#
# 3. Make a box and whisker plot of your data that clearly highlights the mean, median,
# interquartile range, and outliers (if any). (4)

library(ggplot2)

mean_value <- mean(gen_r_data)
ggplot(data = data.frame(x = gen_r_data), aes(y = x)) +
  geom_boxplot() +
  geom_hline(yintercept = mean_value, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Boxplot of Normal Distribution with Mean Line (blue dashed)",
       y = "Value",
       x = "Generated Data") +
  theme_minimal()


# 4. Define a binomial and poisson distribution (2). Describe the major differences and
# provide an example of an associated data type for each (4).


# 5. From the data table below, manually calculate the mean, average deviation, sum of
# squares, degrees of freedom, sample variance, standard deviation, and standard error.
# Clearly show your work for each step. (7)

manual <- c(5,
            7,
            4,
            32,
            6,
            8,
            16,
            50,
            14,
            65,
            3)


mean(manual) #mean
#average deviation?
sum_of_squares <-sum(manual^2) #sum of squares
deg_of_free <- length(manual)-1 #degrees of freedom
var(manual) #sample variance
sd(manual) #standard deviation
standard_error <- sd(manual) / sqrt(length(manual)) #standard error 

