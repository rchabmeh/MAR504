#All code and outputs to be sent to oliver.shipley@stonybrook.edu by end of class.

# MAR504 –Workshop 3 – Pairwise Comparisons

# Part A – Pairwise comparisons
# 1. Read in the datafiles ‘Crab Data.csv’. These data are carapace width estimates (in mm) taken from two populations of horseshoe crabs sampled from the Long Island Sound and New York Bight.
crabs_data <- read.csv("/Users/reneechabot-mehlin/Downloads/CrabData.csv")
# 2A. Determine whether statistically significant differences in average carapace widths exist between the two sampling populations. 

##Normality first:
shapiro.test(crabs_data$Long.Island.Sound) #this is a normal distribution
# Shapiro-Wilk normality test
# data:  crabs_data$Long.Island.Sound
# W = 0.92943, p-value = 0.2676
shapiro.test(crabs_data$New.York.Bight) #this is a normal distribution
# Shapiro-Wilk normality test
# data:  crabs_data$New.York.Bight
# W = 0.96655, p-value = 0.8042

## Similar or equal variance next:
var.test(crabs_data$Long.Island.Sound,crabs_data$New.York.Bight, alternative = "two.sided") 
# F test to compare two variances
# data:  crabs_data$Long.Island.Sound and crabs_data$New.York.Bight
# F = 1.0951, num df = 14, denom df = 14, p-value = 0.8675
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.3676467 3.2617558
# sample estimates:
#   ratio of variances 
# 1.095068 

## T-test last:
t.test(crabs_data$Long.Island.Sound,crabs_data$New.York.Bight,alternative = "two.sided", var.equal = T)
# Two Sample t-test
# data:  crabs_data$Long.Island.Sound and crabs_data$New.York.Bight
# t = 1.3368, df = 28, p-value = 0.1921
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4652668  2.2132668
# sample estimates:
#   mean of x mean of y 
# 33.082    32.208 
t.test(crabs_data$Long.Island.Sound,crabs_data$New.York.Bight,alternative = "two.sided", var.equal = F)
# Welch Two Sample t-test
# data:  crabs_data$Long.Island.Sound and crabs_data$New.York.Bight
# t = 1.3368, df = 27.942, p-value = 0.1921
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.4653911  2.2133911
# sample estimates:
#   mean of x mean of y 
# 33.082    32.208 

##It is not statistically different. 

# 2B. Justify your decision based on statistical assumptions outlined in Tuesday’s lecture. 

##First I ran a normality test to see if both datasets have normal distributions, which they do. 
##This means that I can now test variance to see if their variances are similar or equal which they are.
##Lastly, now I can see using a t-test if the data is statistically different or similar. 
##Both t-test show that probability of t value falling beyond extremes of the critical value in the t-distribution is low (0.1921).
##The difference between the sample means relative to their variability is small as well (1.3368).

# 2C. Run the appropriate tests to illustrate whether data conform to or violate these assumptions.

##Look above.

# 3. Make appropriate plots to effectively visualize your data based on the statistical tests conducted.

boxplot(
  crabs_data$Long.Island.Sound,
  crabs_data$New.York.Bight,
  names = c("Long Island Sound", "New York Bight"),
  main = "Crab data comparison",
  xlab = "Location",
  ylab = "Carapace Width Estimates (mm)"
)

# 4. Undertake the same exercise using ‘FishLengths.csv’. 
#These are fork length estimates (mm) from two sampling populations of juvenile black sea bass sampled from artificial reefs and the continental shelf.
fish_length <- read.csv("/Users/reneechabot-mehlin/Downloads/FishLength.csv")
##Normality first:
shapiro.test(fish_length$Continental.Shelf) #this is a normal distribution
# Shapiro-Wilk normality test
# data:  fish_length$Continental.Shelf
# W = 0.9744, p-value = 0.7568
shapiro.test(fish_length$Artificial.Reef) #this is a normal distribution
# Shapiro-Wilk normality test
# data:  fish_length$Artificial.Reef
# W = 0.9566, p-value = 0.351

## Similar or equal variance next:
var.test(fish_length$Continental.Shelf,fish_length$Artificial.Reef, alternative = "two.sided") #variance is too different for student's t-test
# F test to compare two variances
# data:  fish_length$Continental.Shelf and fish_length$Artificial.Reef
# F = 5.7592, num df = 24, denom df = 24, p-value = 5.964e-05
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   2.537919 13.069316
# sample estimates:
#   ratio of variances 
# 5.759242 

## T-test last:
t.test(fish_length$Continental.Shelf,fish_length$Artificial.Reef,alternative = "two.sided", var.equal = F)
# Welch Two Sample t-test
# data:  fish_length$Continental.Shelf and fish_length$Artificial.Reef
# t = 9.5107, df = 32.091, p-value = 7.411e-11
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   29.40276 45.42760
# sample estimates:
#   mean of x mean of y 
# 66.40555  28.99037 

##It is statistically different. 

# 2B. Justify your decision based on statistical assumptions outlined in Tuesday’s lecture. 

##First I ran a normality test to see if both datasets have normal distributions, which they do. 
##This means that I can now test variance to see if their variances are similar or equal which they are not.
##Lastly, now I can see using a t-test if the data is statistically different or similar. 
##The t-test show that probability of t value falling beyond extremes of the critical value in the t-distribution is really low (7.411e-11).
##The difference between the sample means relative to their variability is large as well (9.5107).

boxplot(
  fish_length$Continental.Shelf,
  fish_length$Artificial.Reef,
  names = c("Continental Shelf", "Artificial Reef"),
  main = "Fish data comparison",
  xlab = "Location",
  ylab = "Fork Length Estimates (mm)"
)
# 5. Undertake the same exercise using ‘SpeciesWeights.csv’. These are average weights (g) taken from two different species of songbird.
species_weight_data <- read.csv("/Users/reneechabot-mehlin/Downloads/SpeciesWeights.csv")
##Normality test first:  
shapiro.test(species_weight_data$Mimus.polyglottos) #this is a normal distribution
# Shapiro-Wilk normality test
# data:  species_weight_data$Mimus.polyglottos
# W = 0.95699, p-value = 0.0665
shapiro.test(species_weight_data$Melanotis.caerulescens) #this is a normal distribution
# Shapiro-Wilk normality test
# data:  species_weight_data$Melanotis.caerulescens
# W = 0.97987, p-value = 0.5464

## Similar or equal variance next:
var.test(species_weight_data$Mimus.polyglottos,species_weight_data$Melanotis.caerulescens, alternative = "two.sided") 
# F test to compare two variances
# data:  species_weight_data$Mimus.polyglottos and species_weight_data$Melanotis.caerulescens
# F = 1.6355, num df = 49, denom df = 49, p-value = 0.08829
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.9280857 2.8819923
# sample estimates:
#   ratio of variances 
# 1.635462 

## T-test last:
t.test(species_weight_data$Mimus.polyglottos,species_weight_data$Melanotis.caerulescens,alternative = "two.sided", var.equal = T)
# Two Sample t-test
# data:  species_weight_data$Mimus.polyglottos and species_weight_data$Melanotis.caerulescens
# t = 6.0685, df = 98, p-value = 2.439e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   4.989545 9.838455
# sample estimates:
#   mean of x mean of y 
# 22.802    15.388 
t.test(species_weight_data$Mimus.polyglottos,species_weight_data$Melanotis.caerulescens,alternative = "two.sided", var.equal = F)
# Welch Two Sample t-test
# data:  species_weight_data$Mimus.polyglottos and species_weight_data$Melanotis.caerulescens
# t = 6.0685, df = 92.615, p-value = 2.797e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   4.987782 9.840218
# sample estimates:
#   mean of x mean of y 
# 22.802    15.388 

##It is statistically different. 

# 2B. Justify your decision based on statistical assumptions outlined in Tuesday’s lecture. 

##First I ran a normality test to see if both datasets have normal distributions, which they do. 
##This means that I can now test variance to see if their variances are similar or equal which they are.
##Lastly, now I can see using a t-test if the data is statistically different or similar. 
##Both t-test show that probability of t value falling beyond extremes of the critical value in the t-distribution is really low (2.797e-08).
##The difference between the sample means relative to their variability is large as well (6.0685).

boxplot(
  species_weight_data$Mimus.polyglottos,
  species_weight_data$Melanotis.caerulescens,
  names = c("Mimus polyglottos", "Melanotis caerulescens"),
  main = "Songbird data comparison",
  xlab = "Species",
  ylab = "Average Weights (g)"
)

# Part B – Wilcoxon-signed rank test
# 1. Generate a dataset that would require you to perform a Wilcoxon signed-rank test. 
#Hint there are functions in R to help you generate this type of data!

gen_data <- data.frame(rpois(100,0.5), rpois(100,2.5))
shapiro.test(gen_data$rpois.100..0.5.) #not normal
# Shapiro-Wilk normality test
# data:  gen_data$rpois.100..0.5.
# W = 0.69442, p-value = 4.005e-13
shapiro.test(gen_data$rpois.100..2.5.) #not normal
# Shapiro-Wilk normality test
# data:  gen_data$rpois.100..2.5.
# W = 0.91991, p-value = 1.391e-05

##What I need to use Wilcoxon_signed rank test: 
# 1. Robust with smaller sample sizes ##yes
# 2. Data are not normally distributed. ##yes
# 3. Data can be ranked or ordinal. ##its not but it can be
# 4. Data do not have to exhibit similar or equal variances ##doesn't have so not testing for it

wilcox.test(gen_data$rpois.100..0.5., gen_data$rpois.100..2.5., data = gen_data, paired = T,alternative = "two.sided" )
# Wilcoxon signed rank test with continuity correction
# data:  gen_data$rpois.100..0.5. and gen_data$rpois.100..2.5.
# V = 75, p-value = 1.401e-14
# alternative hypothesis: true location shift is not equal to 0

# 2. Describe the conditions and associated assumptions that necessitate conducting this test.

##Look above.

# 3. Report the appropriate statistics and generate associated plots.

boxplot(
  gen_data$rpois.100..0.5.,
  gen_data$rpois.100..2.5.,
  names = c("First poisson data", "Second poisson data"),
  main = "Generated Data",
  xlab = "Random Data Names",
  ylab = "Values Generated"
)

# NOTE. Provide clearly articulated figure captions for all figures generated.


