#Assignment 3.1 2014 American Community Survey
# Fox, Andrea
# 11 September 2019

install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
mydata <- read.csv("acs-14-1yr-s0201.csv", header = TRUE, sep = ",")
#1 There is 1 dataframe  and 2 factors as our data types. 136 observations and 8 variables
str(mydata)
nrow(mydata)
ncol(mydata)
HSDegree.hist <- ggplot(mydata, aes(HSDegree))+
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white", binwidth = 1) +
  labs(x = "% Population with HS Degree", y = "Density of Population") +
  stat_function(fun = dnorm, args = list(mean = mean(mydata$HSDegree, na.rm = TRUE), sd = sd(mydata$HSDegree, na.rm = TRUE)), colour = "blue", size = 1)
HSDegree.hist
#4a - I would say this is unimodal because there is really one noticable hump in the data near the 90% mark
#4b - This is not symmetric as there is more data on the left side of the peak than on the rights side
#4c - I would not consider this bell shaped as the data is mostly peaked on the far right side so there is not an even bell curve on either side of the peak
#4d - Normal distribution can resemble the bell shape as well. I wouldn't consider this normal because we have tight distribution between the 85-95% range with several outliers below that 85% mark
#4e - I think the data is skewed. We have a high population at the top percentage, but lots of outliers below the 85% mark with some sitting at 65%
#4g - Normal distribution cannot be used for this as we have a high percentage of the population that have their HS Degree. In order for it to be normal we would expect to have an even amount of distribution on either side of the peak or mean.
install.packages("pastecs")
library(pastecs)
HSDegree.probability <- qplot(sample = mydata$HSDegree, stat = "qq") +
  labs(x = "Density", y = "% Population with HS Degree")
  round(stat.desc(mydata$HSDegree, basic = FALSE, norm = TRUE))
HSDegree.probability
#6a - Based on "Discovering Statistics Using R" this probability plot is not normal because it's not at an incline. It has a noticeable curve to it.
#6b- This plot is skewed to the left because it has the upward curve in the line. 
#8 - 