---
title: "Student Survey - Week 5 Assignment"
author: "Andrea Fox"
date: September 26th 2019
---

  
library(readr)
library(ggplot2)
install.packages("psychometric")
library(psychometric)
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
student_data<- read.csv("student-survey.csv", header = TRUE, sep = ",")
summary(student_data)
ggplot(student_data, aes(x = TimeReading, y = TimeTV, color = Happiness))+
  geom_point()
cov(student_data)
#a - We look at covariance to see the average sum of combined deviations. If the covariance is positive it means one variabel deviates from the mean and the other variable deviates in the same direction. However, a negative covariance indicates that as one variable deviates from the mean the other deviates from the mean in the opposite direction, which is what we're seeing in our data here.
#b - The measurements for the variables are different. TimeTV is looking at minutes, TimeReading is using hours, Happiness looks to be a 1-100 scale (not entirely sure), and Gender is using 1 and 0 to indicate male or female (not sure which is which). Since covariance is not a standardized measurement it can be affected by the different scales of meausrement, so our data here might not be giving us an accurate covariance due to different measurements. 
#c = I'm going to use Kendall for my test type because this is a very small data sample and is good for ordinal data for both variables. I think it will give me negative results though because the numbers seem to be all over the place.
cor.test(student_data, method= "kendall")
cor.test(student_data$TimeReading, student_data$TimeTV, method = "kendall")
cor.test(student_data$TimeReading, student_data$TimeTV, method = "kendall", conf.level = 0.99)
#d4 - The variables are not very related as the correlation coefficient is not very close to 0. I'm not sure if this has anything to do with measurements not being standard, but if you look at the data and even if I took my TimeTV and changed it to minutes there would still be significantly more time spent watching TV than reading.
cor(student_data$TimeReading, student_data$TimeTV)
data.lm = lm(TimeReading ~ TimeTV, data = student_data)
summary(data.lm)$r.squared
#e - The correlation coefficient is just barely considered significant because it's greater than =0.8. If I look at the coefficient of determination though it actually shows a fairly good relationship between the two factors because it's closer to 1.
#f - Based on the analysis it looks like watching tv made an impact on reading if I look at the coefficient of determination. 
install.packages("ggm")
library(ggm)
pcor(c("TimeReading", "Happiness", "Gender"), var(student_data))
#g - The data is still showing a negative leaning meaning there isn't a good relationship between the variables. This makes me think maybe there is no relationship between time spent watching tv having an effect on time spent reading.