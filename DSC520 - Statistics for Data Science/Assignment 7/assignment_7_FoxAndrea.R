---
title: "Housing Data - Week 7 Assignment"
author: "Andrea Fox"
date: "October 12th 2019"
---
  
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
install.packages("readxl")
library(readxl)
library(ggplot2)
library(psychometric)
library(ggm)
library(QuantPsyc)
library(car)
housing_data.df<-data.frame(read_excel("week-7-housing.xlsx"))
str(housing_data.df)
SalePrice_single<-lm(Sale.Price ~ sq_ft_lot, data = housing_data.df)
SalePrice_multiple <-lm(Sale.Price ~ bedrooms + bath_full_count, data = housing_data.df)
summary(SalePrice_single)
summary(SalePrice_multiple)
#c - For SalePrice_single R2 is 0.01435 and adjusted R2 is 0.01428. For SalePrice_multiple R2 is 0.1023 and Adjusted R2 is 0.1022. The larger the R2 number becomes the better correlation between the predicted and observed values of the outcome become, Basically it shows us how well the model predicts the observed data, but we add more variables and it will alwyas go up. In this case we added an additional predictor so it could have gone up becuase of that. The adjusted R2 tells us the loss of predictive power, so adding the additional variable shows us that shringkage has increased.
lm.beta(SalePrice_single)
lm.beta(SalePrice_multiple)
#d - The standardized beta numbers will tell us the number of standard deviations that the outcome will change as a result of one standard deviation change in the predictor. 
confint(SalePrice_single)
confint(SalePrice_multiple)
#e - The tighter the numbers for confidence values are to each other the better representative to the population it is. Here the numbers cross 0 and are not very tight together, so I would say this is not representative of the overall population.
anova(SalePrice_single, SalePrice_multiple)
#f - From the value of F and Pr(>F) it looks like model 2 has significantly improved th efit o fthe model to the data compared to model 2.
housing_data.df$residuals <- resid(SalePrice_multiple)
housing_data.df$standardized.residuals <- rstandard(SalePrice_multiple)
housing_data.df$studentized.residuals <- rstudent(SalePrice_multiple)
housing_data.df$cooks.distance <- cooks.distance(SalePrice_multiple)
housing_data.df$dfbeta <- dfbeta(SalePrice_multiple)
housing_data.df$dffit <- dffits(SalePrice_multiple)
housing_data.df$leverage <- hatvalues(SalePrice_multiple)
housing_data.df$covariance.ratios <- covratio(SalePrice_multiple)
housing_data.df
housing_data.df$large.residual<-housing_data.df$standardized.residuals>2 | housing_data.df$standardized.residuals < -2
sum(housing_data.df$large.residual)
housing_data.df[housing_data.df$large.residual,c("Sale.Price", "sq_ft_lot", "bedrooms", "bath_full_count", "standardized.residuals")]
housing_data.df[housing_data.df$large.residual,c("Sale.Price", "sq_ft_lot", "bedrooms", "bath_full_count", "cooks.distance")]
housing_data.df[housing_data.df$large.residual,c("Sale.Price", "sq_ft_lot", "bedrooms", "bath_full_count", "leverage")]
housing_data.df[housing_data.df$large.residual,c("Sale.Price", "sq_ft_lot", "bedrooms", "bath_full_count", "covariance.ratios")]
#k - For Cook's distance there is one problem for 295 it's cook's distnace is well over 1. 
dwt(SalePrice_multiple)
#l- Independence is not met here because the value is less than 1, which should be cause for concern. Ideally we would want this as close to 2 as possible.Anything below 1 or above 3 is cause for concern.
vif(SalePrice_multiple)
#m - We can safely say there is no collinearity here because the values are not greater than 10 and are all well above 0.2. The VIF is very close to 1, which is why I say there is no collinearity.
plot(housing_data.df$residuals)
plot(housing_data.df$standardized.residuals)
plot(housing_data.df$studentized.residuals)
hist(housing_data.df$residuals)
hist(housing_data.df$standardized.residuals)
hist(housing_data.df$studentized.residuals)
#n - For the plots I would say for the most part they look normal as most of the data is on the straight line, but there are some outliers. Looking at the histograms they all show normal graphs as you have the typical peak/bell curve that's expected for a normal graph.
#o - Overall, I would say this model is not unbiased because not all of the qualifications are met for example we would want this to independent, which earlier we discovered it wasn't.
