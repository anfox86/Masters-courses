#Assignment 2
# Fox, Andrea
# 7 September 2019

install.packages("ggplot2")
library(ggplot2)
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
mydata <- read.csv("scores.csv", header = TRUE, sep = ",")
#1 - Observational units are scores and counts, which gives us the full picture of performance.
#2 - The categorical variable in this assignment is the type of class it can only be regular or sports. The quantitative variable is count and score because they're numerical and becaues they can change.
regular_class <- subset(mydata, Section == "Regular")
sports_class <- subset(mydata, Section == "Sports")
plot(regular_class$Score,regular_class$Count, main="Regular Section",
     xlab="Scores", ylab="Count",
     xlim=c(200, 400), ylim=c(10, 30))
plot(sports_class$Score,sports_class$Count, main="Sport Section",
     xlab="Scores", ylab="Count",
     xlim=c(200, 400), ylim=c(10, 30))
#4a - Looking strictly at plot points the Regular class scored higher than the Sports class. If we look at 300 as our midlevel there are more points on the higher end for the Regular section than the Sports section. 
#4b - Looking at the average scores for each class size (count) each class in the Regular section scored higher than the Sports section. However, there were more students in the Sports section (2).
#4c - The first possible variable that I thought of was ratio of male to female students. I would assume there were more male students in the sports section vs the regular section, which could have an influence on performance.
