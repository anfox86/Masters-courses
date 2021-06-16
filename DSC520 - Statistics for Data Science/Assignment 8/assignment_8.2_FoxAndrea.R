---
title: "Fit Logistic Model to Previous Dataset - Week 8.2 Assignment"
author: "Andrea Fox"
date: "October 20th 2019"
---
  
library(caTools)
library(caret)
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
binaryxy <- read.csv("binary-classifier-data.csv", header = TRUE)
binaryxy$label <- as.factor(binaryxy$label)
split <- sample.split(binaryxy, SplitRatio = 0.8)
split
train <- subset(binaryxy, split == "TRUE")
test <- subset(binaryxy, split == "FALSE")
myModel <- glm(label ~ x + y, data = train, family = 'binomial')
summary(myModel)
res <- predict(myModel, test, type = "response")
res
res <- predict(myModel, train, type = "response")
res
confmatrix <- table(Actual_Value = train$label, Predicted_Value = res > 0.5)
confmatrix
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
install.packages("class")
library(class)
NROW(train)
sqrt(999)
knn.31 <- knn(train = train, test = test, cl = train$label, k = 31)
knn.32 <- knn(train = train, test = test, cl = train$label, k = 32)
ACC.31 <- 100 * sum(test$label == knn.31)/NROW(test$label)
ACC.32 <- 100 * sum(test$label == knn.32)/NROW(test$label)
ACC.31
ACC.32