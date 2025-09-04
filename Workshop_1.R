##### __________________________ Work Shop 1 _____________________________ #####
##### Part A #####
dir <- getwd()
iris_csv <- paste0(dir, "/iris_wrksp_1.csv")
write.csv(iris, iris_csv)
iris_2.0 <- read.csv(iris_csv)
new_df <- data.frame(iris_2.0$Sepal.Length,
                     iris_2.0$Sepal.Width,
                     iris_2.0$Species)

##### Part B #####

#Perform basic descriptive statistics to describe central tendency and
#           dispersion using components of the new iris dataset.
summary(iris_2.0)
sd(iris_2.0$Sepal.Length)
sd(iris_2.0$Sepal.Width)
sd(iris_2.0$Petal.Length)
sd(iris_2.0$Petal.Width)

#Justify your use of certain descriptive statistics based on knowledge of
#                     the underlying distributions.

fishxy_csv <- paste0(dir, "/fishxy.csv")
fishxy <- read.csv(fishxy_csv)

#Perform basic descriptive statistics to describe central tendency and
#           dispersion using components of the new fishxy dataset.
summary(fishxy)
sd(fishxy$ID)
sd(fishxy$Biomass)
sd(fishxy$Salinity)
sd(fishxy$Depth)

#Justify your use of certain descriptive statistics based on knowledge of
#                     the underlying distributions.


# Make four basic plots of the raw (two plots for each dataset) using baseR
# to effectively visualize the data. Accurately assign figure legends!

#boxplot
#per species
library(dplyr)
iris_split <- list()
for (spec in unique(iris_2.0$Species)) {
  iris_split[[spec]] <- dplyr::filter(iris_2.0, Species == spec)
}

for (spec in unique(iris_2.0$Species)) {
  hist(
    x = iris_split[[spec]]$Petal.Length,
    xlab = "Petal Length",
    main = paste0("Iris 2.0 csv file Petal Length for", spec)
  )
  print(paste("This Shapiro test is for", spec))
  print(shapiro.test(iris_split[[spec]]$Petal.Length))
}

for (spec in unique(iris_2.0$Species)) {
  hist(
    x = iris_split[[spec]]$Petal.Width,
    xlab = "Petal Width",
    main = paste0("Iris 2.0 csv file Petal Width for", spec)
  )
  print(paste("This Shapiro test is for", spec))
  print(shapiro.test(iris_split[[spec]]$Petal.Width))
}

for (spec in unique(iris_2.0$Species)) {
  hist(
    x = iris_split[[spec]]$Sepal.Length,
    xlab = "Sepal Length",
    main = paste0("Iris 2.0 csv file Sepal Length for", spec)
  )
  print(paste("This Shapiro test is for", spec))
  print(shapiro.test(iris_split[[spec]]$Sepal.Length))
}

for (spec in unique(iris_2.0$Species)) {
  hist(
    x = iris_split[[spec]]$Sepal.Width,
    xlab = "Sepal Width",
    main = paste0("Iris 2.0 csv file Sepal Width for", spec)
  )
  print(paste("This Shapiro test is for", spec))
  print(shapiro.test(iris_split[[spec]]$Sepal.Width))
}

  boxplot(
    Petal.Width ~ Species,
    iris_2.0,
    xlab = "Species",
    ylab = "Petal Width",
    main = paste("Box Plot for Petal Width")
  )
  
  boxplot(
    Petal.Length ~ Species,
    iris_2.0,
    xlab = "Species",
    ylab = "Petal Length",
    main = paste("Box Plot for Petal Length")
  )
  
  boxplot(
    Sepal.Length ~ Species,
    iris_2.0,
    xlab = "Species",
    ylab = "Sepal Length",
    main = paste("Box Plot for Sepal Length")
  )

  boxplot(
    Sepal.Width ~ Species,
    iris_2.0,
    xlab = "Species",
    ylab = "Sepal Width",
    main = paste("Box Plot for Sepal Width")
  )
  
##### Part C #####

#Explore different datasets and repeat activities A and B.


#Send all final code, results, and plots in a single word file to
#         oliver.shipley@stonybrook.edu by end of class.

##### ____________________________________________________________________ #####
