---
title: "Clustering - Week 9.3 Assignment"
author: "Andrea Fox"
date: "October 27th 2019"
---
  
library(ggplot2)
library(class)
library(caTools)
library(caret)
library(lattice)
install.packages("tidyverse")
library(tidyverse)
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)

setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
clustering_data <- read.csv("clustering-data.csv", header = TRUE)
summary(clustering_data)
# plotting the dataset
clustering <- ggplot(clustering_data, aes(x = x, y = y)) +
  geom_point() + geom_smooth() + geom_jitter()
clustering
# Fit the dataset using the k-means algorithm
df <- clustering_data
df <- na.omit(df)
df <- scale(df)
head(df)
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k3 <- kmeans(df, centers = 3, nstart = 25)
str(k3)
k4 <- kmeans(df, centers = 4, nstart = 25)
str(k4)
k5 <- kmeans(df, centers = 5, nstart = 25)
str(k5)
k6 <- kmeans(df, centers = 6, nstart = 25)
str(k6)
k7 <- kmeans(df, centers = 7, nstart = 25)
str(k7)
k8 <- kmeans(df, centers = 8, nstart = 25)
str(k8)
k9 <- kmeans(df, centers = 9, nstart = 25)
str(k9)
k10 <- kmeans(df, centers = 10, nstart = 25)
str(k10)
k11 <- kmeans(df, centers = 11, nstart = 25)
str(k11)
k12 <- kmeans(df, centers = 12, nstart = 25)
str(k12)
# Plot k valuesk2$cluster <- as.factor(k2$cluster) - was not able to figure out how to plot the models
# Unable to finish assignment. Tried following https://uc-r.github.io/kmeans_clustering#distance but couldn't understand what I was seeing and started feeling sick
