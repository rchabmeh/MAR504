##### Part A #####
dir <- getwd()
iris_csv <-paste0(dir,"/iris_wrksp_1.csv")
write.csv(iris, iris_csv)
iris_2.0 <- read.csv(iris_csv)
new_df <-data.frame(iris_2.0$Sepal.Length,iris_2.0$Sepal.Width,iris_2.0$Species)

##### Part B #####

#Perform basic descriptive statistics to describe central tendency and 
#           dispersion using components of the new iris dataset.
summary(iris_2.0)
lm( x ~ y, data = iris_2.0)
glm( x ~ y, data = iris_2.0)

#Justify your use of certain descriptive statistics based on knowledge of
#                     the underlying distributions.


fishxy_csv <-paste0(dir,"/fishxy.csv")
fishxy <- read.csv(fishxy_csv)

#Perform basic descriptive statistics to describe central tendency and 
#           dispersion using components of the new fishxy dataset.
summary(fishxy)
lm( x ~ y, data = fishxy)
glm( x ~ y, data = iris_2.0)

#Justify your use of certain descriptive statistics based on knowledge of
#                     the underlying distributions.


# Make four basic plots of the raw (two plots for each dataset) using baseR
# to effectively visualize the data. Accurately assign figure legends!


##### Part C #####

#Explore different datasets and repeat activities A and B.



#Send all final code, results, and plots in a single word file to
#         oliver.shipley@stonybrook.edu by end of class.

