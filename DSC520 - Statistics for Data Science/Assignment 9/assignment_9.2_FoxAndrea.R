---
title: "Introduction to Machine Learning - Week 9.2 Assignment"
author: "Andrea Fox"
date: "October 25th 2019"
---
  
library(ggplot2)
library(class)
library(caTools)
library(caret)
library(lattice)
setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
binaryxy <- read.csv("binary-classifier-data.csv", header = TRUE)
trinaryxy <-read.csv("trinary-classifier-data.csv", header = TRUE)
#Create a scatterplot for both sets of data
binary <- ggplot(binaryxy, aes(x, y))+
  geom_point()+geom_smooth() 
binary
trinary <- ggplot(trinaryxy, aes(x, y)) +
  geom_point() + geom_smooth()
trinary
#Nearest neighbor looking at accuracy specifically for binaryxy
binaryxy$label <- as.factor(binaryxy$label)
split <- sample.split(binaryxy, SplitRatio = 0.8)
split
train <- subset(binaryxy, split == "TRUE")
test <- subset(binaryxy, split == "FALSE")
biModel <- glm(label ~ x + y, data = train, family = 'binomial')
summary(biModel)
res <- predict(biModel, test, type = "response")
res
res <- predict(biModel, train, type = "response")
res
confmatrix <- table(Actual_Value = train$label, Predicted_Value = res > 0.5)
confmatrix
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
NROW(train)
sqrt(999)
knn.3 <- knn(train = train, test = test, cl = train$label, k = 3)
ACC.3 <- 100 * sum(test$label == knn.3)/NROW(test$label)
knn.5 <- knn(train = train, test = test, cl = train$label, k = 5)
ACC.5 <- 100 * sum(test$label == knn.5)/NROW(test$label)
knn.10 <- knn(train = train, test = test, cl = train$label, k = 10)
ACC.10 <- 100 * sum(test$label == knn.10)/NROW(test$label)
knn.15 <- knn(train = train, test = test, cl = train$label, k = 15)
ACC.15 <- 100 * sum(test$label == knn.15)/NROW(test$label)
knn.20 <- knn(train = train, test = test, cl = train$label, k = 20)
ACC.20 <- 100 * sum(test$label == knn.20)/NROW(test$label)
knn.25 <- knn(train = train, test = test, cl = train$label, k = 25)
ACC.25 <- 100 * sum(test$label == knn.25)/NROW(test$label)
ACC.3
ACC.5
ACC.10
ACC.15
ACC.20
ACC.25
bin_acc <- data.frame(KValue=c(3),Accuracy=c(ACC.3))
bin_acc <- rbind(bin_acc, c(5, ACC.5))
bin_acc <- rbind(bin_acc, c(10, ACC.10))
bin_acc <- rbind(bin_acc, c(15, ACC.15))
bin_acc <- rbind(bin_acc, c(20, ACC.20))
bin_acc <- rbind(bin_acc, c(25, ACC.25))
# Plot for binary accuracy data
binary.plot <- ggplot(bin_acc, aes(x = KValue, y = Accuracy)) +  
  geom_point() + geom_smooth()
binary.plot
# Nearest neighbor looking at accuracy specifically for trinaryxy
trinaryxy$label <- as.factor(binaryxy$label)
split <- sample.split(binaryxy, SplitRatio = 0.8)
split
train <- subset(trinaryxy, split == "TRUE")
test <- subset(trinaryxy, split == "FALSE")
triModel <- glm(label ~ x + y, data = train, family = 'binomial')
summary(triModel)
res <- predict(triModel, test, type = "response")
res
res <- predict(triModel, train, type = "response")
res
confmatrix <- table(Actual_Value = train$label, Predicted_Value = res > 0.5)
confmatrix
(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
NROW(train)
sqrt(1045)
knn.3 <- knn(train = train, test = test, cl = train$label, k = 3)
ACC.3 <- 100 * sum(test$label == knn.3)/NROW(test$label)
knn.5 <- knn(train = train, test = test, cl = train$label, k = 5)
ACC.5 <- 100 * sum(test$label == knn.5)/NROW(test$label)
knn.10 <- knn(train = train, test = test, cl = train$label, k = 10)
ACC.10 <- 100 * sum(test$label == knn.10)/NROW(test$label)
knn.15 <- knn(train = train, test = test, cl = train$label, k = 15)
ACC.15 <- 100 * sum(test$label == knn.15)/NROW(test$label)
knn.20 <- knn(train = train, test = test, cl = train$label, k = 20)
ACC.20 <- 100 * sum(test$label == knn.20)/NROW(test$label)
knn.25 <- knn(train = train, test = test, cl = train$label, k = 25)
ACC.25 <- 100 * sum(test$label == knn.25)/NROW(test$label)
ACC.3
ACC.5
ACC.10
ACC.15
ACC.20
ACC.25
tri_acc <- data.frame(KValue=c(3),Accuracy=c(ACC.3))
tri_acc <- rbind(tri_acc, c(5, ACC.5))
tri_acc <- rbind(tri_acc, c(10, ACC.10))
tri_acc <- rbind(tri_acc, c(15, ACC.15))
tri_acc <- rbind(tri_acc, c(20, ACC.20))
tri_acc <- rbind(tri_acc, c(25, ACC.25))
# Plot for trinary accuracy data
trinary.plot <- ggplot(tri_acc, aes(x = KValue, y = Accuracy)) +  
  geom_point() + geom_smooth()
trinary.plot
