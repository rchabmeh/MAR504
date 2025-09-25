#All code and outputs to be sent to oliver.shipley@stonybrook.edu by end of class.
# MAR504 –Workshop 4 – Analysis of Variance (ANOVA)
# Part A – Pairwise comparisons_______________________________________________________________#
# 1. Read in the datafile ‘Species_Length.csv’. This comprises fork length estimates for four
# species of tropical freshwater fish.

trop_fish <- read.csv("/Users/reneechabot-mehlin/Downloads/Species_Lengths.csv")
trop_fish$X <- NULL

# 2. Format the data into a format that is required to run an ANOVA. Hint: download the
# ‘tidyr’ package and utilize the ‘gather’ function to manipulate the data frame from four
# columns into two.

library(tidyr)
fish <- gather(data = trop_fish,key = "Species", value =  measurement)

# 3. Explore the data for potential normality and heterogeneity of variance issues. Make some
# plots to visualize the data and report associated descriptive statistics.

mean(trop_fish$Species1) ## 43.94118
mean(trop_fish$Species2) ## 31.19204
mean(trop_fish$Species3) ## 99.9129
mean(trop_fish$Species4) ## 35.11937

mean(fish$measurement) ## 52.54137

shapiro.test(trop_fish$Species1) ## p-value = 0.5188; normal 
shapiro.test(trop_fish$Species2) ## p-value = 0.1197; normal
shapiro.test(trop_fish$Species3) ## p-value = 0.4021; normal
shapiro.test(trop_fish$Species4) ## p-value = 0.4427; normal

install.packages("rstatix")
library(rstatix)

identify_outliers(fish,variable = "measurement")
# Species     measurement is.outlier  is.extreme 
# <0 rows> (or 0-length row.names)

boxplot(
  trop_fish$Species1,
  trop_fish$Species2,
  trop_fish$Species3,
  trop_fish$Species4,
  xlab = "Species",
  ylab = "Fork length",
  main = "Tropical Fish Box Plots",
  names = c("1","2","3","4")
)

par(mfrow = c(2,2))
hist(trop_fish$Species1, main = "Historgram of Species 1")
hist(trop_fish$Species2, main = "Historgram of Species 2")
hist(trop_fish$Species3, main = "Historgram of Species 3")
hist(trop_fish$Species4, main = "Historgram of Species 4")

# 4. Run a one-way ANOVA to determine whether statistically significant differences exist
# between mean lengths of each species.

 res.aov <-aov(measurement ~ Species, data = fish)
 summary(res.aov)
 # Df Sum Sq Mean Sq F value Pr(>F)    
 # Species      3  61547   20516    2197 <2e-16 ***
 #Residuals   76    710       9                   
 # ---
 #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
 model_residuals <- res.aov$residuals
 shapiro.test(model_residuals) # p-value = 0.2222; normal
 
 par(mfrow = c(2,2))
 plot(res.aov)
 
 
# 5. Report the associated statistics like you would in a scientific paper (e.g., Fdf and p value).
 
## F(3,76) = 2.197, p < 0.001
 
# 6. Explain how each of the degrees of freedom are calculated and which components of the
# variance they represent (e.g., within or between group?).

## The degrees of freedom are calculated by: 
## df(within) = N - a 
## df(among) = a - 1 
## Where a is the number of groups and N is the total number of observations in each group.
 a = 4
 N = 80
## df(total) = df(among) + df(within) 
dftotal = (a - 1) + (80 - a) #79
 
# 7. Check the fitted ANOVA model for violations of normality and heterogeneity of variance.
# Make the appropriate plots of the fitted residuals. Run necessary normality and variance
# tests.
 
## It all looks ok! 
 
# Part B_____________________________________________________________________________________#
# 8. Run a post-hoc comparison to determine where statistically significant differences lie
# between groups. Justify your use of post-hoc test.

 ## I am using Tukey's as it makes all pairwise comparisons simultaneously and also offers a good balance between controlling for Type I error and retaining statistical power.
TUKEY <- TukeyHSD(res.aov, conf.level =  0.95)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = measurement ~ Species, data = fish)
# 
# $Species
# diff        lwr        upr     p adj
# Species2-Species1 -12.749141 -15.287244 -10.211038 0.0000000
# Species3-Species1  55.971724  53.433621  58.509827 0.0000000
# Species4-Species1  -8.821813 -11.359916  -6.283710 0.0000000
# Species3-Species2  68.720865  66.182762  71.258968 0.0000000
# Species4-Species2   3.927328   1.389225   6.465431 0.0006605
# Species4-Species3 -64.793537 -67.331640 -62.255434 0.0000000

 
# 9. Report the comparisons in a table that clearly illustrates the p values.
T_matrix <- TUKEY[["Species"]]
p_adj <- T_matrix[ , c(4)]
T_matrix <- as.data.frame(p_value)


# 10. Make the appropriate plots using letters to highlight statistically significant differences at
# alpha = 0.05 (remember these p values are now adjusted for multiple comparisons).
p<-boxplot(
  trop_fish$Species1,
  trop_fish$Species2,
  trop_fish$Species3,
  trop_fish$Species4,
  names = c("A","B","C","D"),
  xlab = "Species",
  ylab = "Fork length",
  main = "Tropical Fish Box Plots"
)

names <- c("A","B","C","D")
text( 
  x=c(1:4), 
  y=p$stats[nrow(p$stats),] + 2.5, 
  paste(names)  
)
# NOTE. Provide clearly articulated figure captions for all figures generated.