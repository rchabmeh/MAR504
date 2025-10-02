#All code and outputs to be sent to oliver.shipley@stonybrook.edu by end of class.
# MAR504 –Workshop 5 – Correlation
# Install the following packages before undertaking the workshop.

# install.packages("MASS")
# install.packages("mnormt")
# install.packages("MVN")
# install.packages("corrplot")

#Code for assessing bivariate and multivariate distributions
# mvn_result <- mvn(yourdata, desc = TRUE)
# print(mvn_result)

# Part A. Correlation Tests
# Read in the datafile ‘Dogfish_condition.csv’. This comprises total length and morphometric body condition estimates for smooth dogfish (Mustelus canis) sampled from the New York Bight.
dogfish <- read.csv("/Users/reneechabot-mehlin/Downloads/Dogfish_condition.csv")
# Make a scatterplot to visualize the data.
plot(
  dogfish$TL,
  dogfish$Condition,
  main = "Scatterplot of Dogfish Condition",
  xlab = "TL (cm)",
  ylab = "Condition",
  pch = 20
)
# Determine whether the data conforms to a bivariate normal distribution.
#Hint use the R package ‘MVN’. Report the appropriate statistics.
library("MVN")
mvn_dogfish <- mvn(dogfish, desc = T)
# $multivariate_normality
# Test Statistic p.value     Method      MVN
# 1 Henze-Zirkler     0.633   0.133 asymptotic ✓ Normal

# $univariate_normality
# Test                  Variable Statistic p.value Normality
# 1 Anderson-Darling        TL     0.664   0.073  ✓ Normal
# 2 Anderson-Darling Condition     0.413   0.314  ✓ Normal

# $descriptive
# Variable  n    Mean Std.Dev Median    Min     Max   25th    75th    Skew Kurtosis
# 1 TL    26  100.719   8.706 97.550 89.000 117.000 94.375 106.525 0.466    1.963
# 2 Co.   26   0.908   0.089  0.894  0.764   1.087  0.836   0.985 0.352    2.102

# Run the appropriate correlation test based on your findings and report the direction and strength of the correlation (if any). Report the appropriate statistics.
correl_pear <- cor.test(dogfish$TL, dogfish$Condition, method = "pearson")
# Pearson's product-moment correlation
#
# data:  dogfish$TL and dogfish$Condition
# t = 5.4183, df = 24, p-value = 1.447e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4972920 0.8771017
# sample estimates:
#       cor
# 0.7417608

# Read in the datafile ‘Dogfish_Length.csv’. This comprises fork length and total length estimates for smooth dogfish sampled from the New York Bight.
# Repeat parts 2-4 and report all appropriate statistics.
dogfish_length <- read.csv("/Users/reneechabot-mehlin/Downloads/Dogfish_Length.csv")
plot(
  dogfish_length$FL,
  dogfish_length$TL,
  main = "Scatterplot of Dogfish Length",
  xlab = "FL (cm)",
  ylab = "TL (cm)",
  pch = 20
)

mvn_dogfish_length <- mvn(dogfish_length, desc = T)
# $multivariate_normality
# Test Statistic p.value     Method          MVN
# 1 Henze-Zirkler     2.966  <0.001 asymptotic ✗ Not normal
#
# $univariate_normality
# Test Variable Statistic p.value Normality
# 1 Anderson-Darling       FL     0.468   0.245  ✓ Normal
# 2 Anderson-Darling       TL     0.519   0.182  ✓ Normal
#
# $descriptives
# Variable  n   Mean Std.Dev Median  Min Max 25th 75th   Skew Kurtosis
# 1     FL 93 78.591  12.327   79.0 51.6 104 71.0 85.4 -0.027    2.873
# 2     TL 93 90.227  13.935   90.1 60.0 129 82.5 97.5  0.029    3.206

correl_spear <- cor.test(dogfish_length$FL, dogfish_length$TL, method = "spearman")

#  Spearman's rank correlation rho
#
# data:  dogfish_length$FL and dogfish_length$TL
# S = 950.3, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho
# 0.9929106

# Part B. Making a correlation matrix and correlogram
# Read in the datafile ‘Heightdata.csv’. This represents height measurements of a school class undertaken by four different methods.
heights <- read.csv("/Users/reneechabot-mehlin/Downloads/Heightdata.csv")
# Determine whether data exhibit a multivariate normal distribution. Hint. This is exactly the same code as for bivariate normal!
mvn_heights <- mvn(heights, desc = T)
# $multivariate_normality
# Test Statistic p.value     Method      MVN
# 1 Henze-Zirkler     0.863   0.328 asymptotic ✓ Normal
#
# $univariate_normality
# Test Variable Statistic p.value Normality
# 1 Anderson-Darling        X     0.535   0.163  ✓ Normal
# 2 Anderson-Darling   Height     0.458   0.253  ✓ Normal
# 3 Anderson-Darling  Height2     0.382   0.386  ✓ Normal
# 4 Anderson-Darling  Height3     0.225   0.811  ✓ Normal
# 5 Anderson-Darling  Height4     0.441   0.279  ✓ Normal
#
# $descriptives
# Variable  n    Mean Std.Dev  Median     Min     Max    25th    75th   Skew     Kurtosis
# 1        X 50  25.500  14.577  25.500   1.000  50.000  13.250  37.750  0.000    1.799
# 2   Height 50 157.869   5.135 158.410 144.638 168.002 155.393 160.330 -0.160    2.805
# 3  Height2 50 157.776   9.048 158.474 140.543 175.940 150.878 163.860 -0.125    2.253
# 4  Height3 50 157.222  11.903 157.867 131.269 180.830 149.713 165.106 -0.241    2.482
# 5  Height4 50 157.933   5.085 158.090 144.298 169.075 155.053 160.215 -0.141    3.025

# Make a correlation matrix table using the appropriate correlation test.
correl_matrix <- cor(heights,method = "pearson")
correl_matrix <- round(correl_matrix, 3)
#            X   Height  Height2 Height3   Height4
# X        1.000  0.212  -0.056   0.095   0.230
# Height   0.212  1.000   0.460   0.605   0.981
# Height2 -0.056  0.460   1.000   0.452   0.467
# Height3  0.095  0.605   0.452   1.000   0.594
# Height4  0.230  0.981   0.467   0.594   1.000

# Make a correlogram to visualize your findings.
library(corrplot)
corrplot(
  correl_matrix,
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  mar = c(0,0,2,0),  
  main = "Correlation Matrix of Heights"
)

# NOTE. Provide clearly articulated figure captions for all figures generated.





###EXTRA
plot(
  dogfish$TL,
  dogfish$Condition,
  main = "Scatterplot of Dogfish Condition",
  xlab = "TL (cm)",
  ylab = "Condition",
  pch = 20
)
model <- lm(Condition ~ TL, data = dogfish)
abline(model, col = "red", lwd = 2)

plot(
  dogfish_length$FL,
  dogfish_length$TL,
  main = "Scatterplot of Dogfish Length",
  xlab = "FL (cm)",
  ylab = "TL (cm)",
  pch = 20
)
model <- lm(TL ~ FL, data = dogfish_length)
abline(model, col = "red", lwd = 2)