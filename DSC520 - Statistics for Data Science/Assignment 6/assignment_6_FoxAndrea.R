---
title: "GSS 2016 Survey Data - Week 6 Assignment"
author: "Andrea Fox"
date: October 3rd 2019
---
  
library(ggplot2)
library(psychometric)
library(ggm)
library(readr)
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
gss_data<- read.csv("gss-2016.csv", header = TRUE, sep = ",")

#Part 1 - "Is there a significant relationship between the number of siblings a survey respondent has and number of his or her children?"  
ggplot(gss_data, aes(x = SIBS, y = CHILDS))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_jitter()
cov(gss_data$SIBS, gss_data$CHILDS, use = "complete.obs")
#1b - Covariance looks at the relationship of two variables. Our covariance is positive, which means as one variable moves in one direction the other moves in the same direction as well.
#1c - I'm going to choose Spearman for my correlation method. The data does not look parametric to me and it's a larger dataset even though the graph doesn't display that. Many of the points are overlapping each other. I predict that the correlation will be fairly close to 0 as they seem to be pretty related from my analysis thus far.
cor.test(gss_data$SIBS, gss_data$CHILDS, method = "spearman")
#1d - Since the p-value is less thatn 0.5 we would reject our null hypothesis in this instance.
cor(gss_data$SIBS, gss_data$CHILDS)
data.lm<- lm(SIBS ~ CHILDS, data = gss_data)
summary(data.lm)$r.squared
#1e - The correltaion coefficient is showing us there is a very weak relationship because the value is fairly close to 0. The coefficient of determination is the square root of correlation coefficient. Here is shows that about 20% of the dependent variable is predicted by the independent variable.
#1f - Based on my analysis there does not seem to be any relationship between the number of siblings and the number of children.
gssdataSIBS.hist<- ggplot(gss_data, aes(SIBS))+
  geom_histogram(aes(y = ..count..), colour = "black", fill = "white", binwidth = 1)+
  stat_function(fun = dnorm, args = list(mean = mean(gss_data$SIBS, na.rm = TRUE), sd = sd(gss_data$SIBS, na.rm = TRUE)), colour = "blue", size = 1)
gssdataSIBS.hist
gssdataCHILDS.hist<- ggplot(gss_data, aes(CHILDS))+
  geom_histogram(aes(y = ..count..), colour = "black", fill = "white", binwidth = 1)+
  stat_function(fun = dnorm, args = list(mean = mean(gss_data$CHILDS, na.rm = TRUE), sd = sd(gss_data$CHILDS, na.rm = TRUE)), colour = "blue", size = 1)
gssdataCHILDS.hist
#1g - The scores for both SIBS and CHILDS are skewed to the right. Althought CHILDS isn't as dramatic looking as SIBS. For CHILDS it looks like people with no children or 2 children have the highest values, and for siblings it looks like 2 siblings had the highest value.
pcor(c("SIBS", "CHILDS", "SEX"), var(gss_data, use = "pairwise.complete.obs"))
#1h - The partial correlation for this is positive, but still closer to 0 showing there isn't much of a relationship between our variables.

#Part 2
install.packages("car")
library(car)
install.packages("QuantPsyc")
library(QuantPsyc)
fit_coeffs <- lm(formula = CHILDS ~ SIBS, data = gss_data)
summary(fit_coeffs)
sqrt(0.03954)
#2b - The intercept is 1.467767, the slope is 0.103577, the coefficient of determination is 0.03921, and coefficient correlation is square root of 0.03954 which is 0.1988467
#2c - What we're seeing is that siblings can only account for 3.9% or rounded up to 4% of the variation in children. This means that 96% cannot be explained by the child count alone.
#2d - The F-ratio we're seeing is significant because the value labelled p-value is less than .001, which tells us that there is less than a 0.1% chance thatn F-ratio this large would happen if the null hypothesis were true.
#2e - number of children = 0.1036*3 + 1.4678. This predicts we will have 1.7786 children if we have 3 siblings.
#2f - number of children = 0.1036*0 + 1.4678. This predicts we will have 1.4678 children if we have 0 siblings.
