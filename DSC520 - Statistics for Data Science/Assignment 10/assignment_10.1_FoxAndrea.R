---
title: "Section 2 - Week 10"
author: "Andrea Fox"
date: "October 28th 2019"
---
install.packages("tinytex")
library(tinytex)
library(devtools)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(caTools)
library(class)
library(caret)
library(lattice)
library(ggm)

setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
#Import the raw data and create initial dataframe
kcpd.18 <- read.csv("KCPD_Crime_Data_2018.csv", header = TRUE)
glimpse(kcpd.18)
kcpd.17 <- read.csv("KCPD_Crime_Data_2017.csv", header = TRUE)
glimpse(kcpd.17)
kcpd.16 <- read.csv("KCPD_Crime_Data_2016.csv", header = TRUE)
glimpse(kcpd.16)

#Removing columns that don't match up
kcpd18 = subset(kcpd.18, select = -c(Location))
names(kcpd18)
kcpd17 = subset(kcpd.17, select = -c(Location))
names(kcpd17)
kcpd16 = subset(kcpd.16, select = -c(Latitude, Longitude, Location.1))
names(kcpd16)

#Combining the 3 seperate dataframes into 1
kcpd.data <- rbind(kcpd18, kcpd17, kcpd16)
summary(kcpd.data)

#At this point I want to create a subset of the data to narrow down my data.
myvars <- c('Report_No', 'Reported_Date', 'Reported_Time', 'Offense', 'Description', 'Zip.Code', 'Area','Firearm.Used.Flag')
main_kcpd <- kcpd.data[,myvars]
summary(main_kcpd)

myvars <- c('Report_No', 'Reported_Date', 'Reported_Time', 'Offense', 'Description', 'Zip.Code', 'Area','Firearm.Used.Flag')
main_kcpd16 <- kcpd16[,myvars]
summary(main_kcpd16)

myvars <- c('Report_No', 'Reported_Date', 'Reported_Time', 'Offense', 'Description', 'Zip.Code', 'Area','Firearm.Used.Flag')
main_kcpd17 <- kcpd17[,myvars]
summary(main_kcpd17)

myvars <- c('Report_No', 'Reported_Date', 'Reported_Time', 'Offense', 'Description', 'Zip.Code', 'Area','Firearm.Used.Flag')
main_kcpd18 <- kcpd18[,myvars]
summary(main_kcpd18)

#Remove NA's from new dataset
na.omit(main_kcpd)
summary(main_kcpd)
na.omit(main_kcpd16)
na.omit(main_kcpd17)
na.omit(main_kcpd18)
#Need to seperate out the column of Reported Date to use for plotting
cln_kcpd <- separate(main_kcpd, Reported_Date, c("Month", "Day", "Year"))
head(cln_kcpd)
glimpse(cln_kcpd)

cln_kcpd16 <- separate(main_kcpd16, Reported_Date, c("Month", "Day", "Year"))
head(cln_kcpd16)

cln_kcpd17 <- separate(main_kcpd17, Reported_Date, c("Month", "Day", "Year"))
head(cln_kcpd17)

cln_kcpd18 <- separate(main_kcpd18, Reported_Date, c("Month", "Day", "Year"))
head(cln_kcpd18)
#Need to separate out the column of Reported_Time to make plotting by hour easier
cln_kcpd1 <- separate(cln_kcpd, Reported_Time, c("Hour", "Minute"))
head(cln_kcpd1)

cln_16kcpd <- separate(cln_kcpd16, Reported_Time, c("Hour", "Minute"))
head(cln_16kcpd)

cln_17kcpd <- separate(cln_kcpd17, Reported_Time, c("Hour", "Minute"))
head(cln_17kcpd)

cln_18kcpd <- separate(cln_kcpd18, Reported_Time, c("Hour", "Minute"))
head(cln_18kcpd)
#Review data
glimpse(cln_kcpd1)
str(cln_kcpd1)
str(cln_16kcpd)
str(cln_17kcpd)
str(cln_18kcpd)

#Plot histogram of crime using Offense Code
ggplot(cln_kcpd1, aes(Offense)) + 
  geom_histogram(color = "black", binwidth = 10, aes(y=..count..)) +
  labs(title = "KCPD Crime by Offense Code", x = "Offense Code", y = "Count") 
ggplot(cln_16kcpd, aes(Offense)) +
  geom_histogram(color = "black", binwidth = 10, aes(y=..count..)) +
  labs(title = "KCPD Crime by Offense Code in 2016", x = "Offense Code", y = "Count")
ggplot(cln_17kcpd, aes(Offense)) +
  geom_histogram(color = "black", binwidth = 10, aes(y=..count..)) +
  labs(title = "KCPD Crime by Offense Code in 2017", x = "Offense Code", y = "Count")
ggplot(cln_18kcpd, aes(Offense)) +
  geom_histogram(color = "black", binwidth = 10, aes(y=..count..)) +
  labs(title = "KCPD Crime by Offense Code in 2018", x = "Offense Code", y = "Count")

#Plot histogram of whether Firearm was Used
ggplot(cln_kcpd1, aes(as.factor(x=Firearm.Used.Flag))) +
  geom_histogram(fill = "black", stat = "count", aes(y=..count..)) +
  labs(title="Firearm Used", x = "Firearm Flag", y = "Count")
ggplot(cln_16kcpd, aes(as.factor(x=Firearm.Used.Flag))) +
  geom_histogram(fill = "black", stat = "count", aes(y=..count..)) +
  labs(title="Firearm Used in 2016", x = "Firearm Flag", y = "Count")
ggplot(cln_17kcpd, aes(as.factor(x=Firearm.Used.Flag))) +
  geom_histogram(fill = "black", stat = "count", aes(y=..count..)) +
  labs(title="Firearm Used in 2017", x = "Firearm Flag", y = "Count")
ggplot(cln_18kcpd, aes(as.factor(x=Firearm.Used.Flag))) +
  geom_histogram(fill = "black", stat = "count", aes(y=..count..)) +
  labs(title="Firearm Used in 2018", x = "Firearm Flag", y = "Count")


#Crime data by Zip code 
ggplot(cln_kcpd1, aes(as.factor(x=Zip.Code))) +
  geom_histogram(fill="dark green", binwidth = 5, stat = "count", aes(y = ..count..)) +
  labs(x="Zip Codes", y="Offense", title="Records By Zip Code") 
ggplot(cln_16kcpd, aes(as.factor(x=Zip.Code))) +
  geom_histogram(fill="dark green", binwidth = 5, stat = "count", aes(y = ..count..)) +
  labs(x="Zip Codes", y="Offense", title="Records By Zip Code in 2016") 
ggplot(cln_17kcpd, aes(as.factor(x=Zip.Code))) +
  geom_histogram(fill="dark green", binwidth = 5, stat = "count", aes(y = ..count..)) +
  labs(x="Zip Codes", y="Offense", title="Records By Zip Code in 2017") 
ggplot(cln_18kcpd, aes(as.factor(x=Zip.Code))) +
  geom_histogram(fill="dark green", binwidth = 5, stat = "count", aes(y = ..count..)) +
  labs(x="Zip Codes", y="Offense", title="Records By Zip Code in 2018") 

#Crime data by Year
ggplot(cln_kcpd1, aes(as.factor(x=Year))) + 
  geom_histogram(fill = "green", binwidth = 2, stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Year", x = "Year", y = "Count")
#Crime data by Month
ggplot(cln_kcpd1, aes(as.factor(x= Month))) + 
  geom_bar(fill = "blue", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Month", x = "Month", y = "Count")
ggplot(cln_16kcpd, aes(as.factor(x= Month))) + 
  geom_bar(fill = "blue", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Month in 2016", x = "Month", y = "Count")
ggplot(cln_17kcpd, aes(as.factor(x= Month))) + 
  geom_bar(fill = "blue", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Month in 2017", x = "Month", y = "Count")
ggplot(cln_18kcpd, aes(as.factor(x= Month))) + 
  geom_bar(fill = "blue", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Month in 2018", x = "Month", y = "Count")

#Crime data by hour
ggplot(cln_kcpd1, aes(as.factor(x=Hour))) +
  geom_bar(fill= "red", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour", x = "Hour", y = "Count")
ggplot(cln_16kcpd, aes(as.factor(x=Hour))) +
  geom_bar(fill= "red", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour in 2016", x = "Hour", y = "Count")
ggplot(cln_17kcpd, aes(as.factor(x=Hour))) +
  geom_bar(fill= "red", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour in 2017", x = "Hour", y = "Count")
ggplot(cln_18kcpd, aes(as.factor(x=Hour))) +
  geom_bar(fill= "red", stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour in 2018", x = "Hour", y = "Count")

#Crime in KC, comparing crime using firearm to no firearm used
ggplot(cln_kcpd1, aes(x = Offense, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 20) +
  labs(title = "Crime in KC with/without Firearms", x = "Offense", y = "Count")
ggplot(cln_16kcpd, aes(x = Offense, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 20) +
  labs(title = "Crime in KC with/without Firearms in 2016", x = "Offense", y = "Count")
ggplot(cln_17kcpd, aes(x = Offense, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 20) +
  labs(title = "Crime in KC with/without Firearms in 2017", x = "Offense", y = "Count")
ggplot(cln_18kcpd, aes(x = Offense, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 20) +
  labs(title = "Crime in KC with/without Firearms in 2018", x = "Offense", y = "Count")

#Crime in KC by month using Firearm or no Firearm
ggplot(cln_kcpd1, aes(x = Month, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title= "Crime in KC by Month/Firearm Used", x = "Month", y = "Count")
ggplot(cln_16kcpd, aes(x = Month, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title= "Crime in KC by Month/Firearm Used in 2016", x = "Month", y = "Count")
ggplot(cln_17kcpd, aes(x = Month, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title= "Crime in KC by Month/Firearm Used in 2017", x = "Month", y = "Count")
ggplot(cln_18kcpd, aes(x = Month, fill = Firearm.Used.Flag)) + 
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title= "Crime in KC by Month/Firearm Used in 2018", x = "Month", y = "Count")

#Crime in KC by Year using Firearm or no Firearm
ggplot(cln_kcpd1, aes(x = Year, fill = Firearm.Used.Flag)) +
  geom_histogram(binwidth = 5, stat = "count", aes(y= ..count..)) +
  labs(title = "Crime in KC by Year/Firearm Used", x = "Year", y = "Count")
#Crime in KC by Hour using Firearm or no Firearm
ggplot(cln_kcpd1, aes(x= Hour, fill = Firearm.Used.Flag)) +
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour/Firearm Used", x = "Hour", y = "Count")
ggplot(cln_16kcpd, aes(x= Hour, fill = Firearm.Used.Flag)) +
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour/Firearm Used in 2016", x = "Hour", y = "Count")
ggplot(cln_17kcpd, aes(x= Hour, fill = Firearm.Used.Flag)) +
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour/Firearm Used in 2017", x = "Hour", y = "Count")
ggplot(cln_18kcpd, aes(x= Hour, fill = Firearm.Used.Flag)) +
  geom_histogram(binwidth = 5, stat = "count", aes(y=..count..)) +
  labs(title = "Crime in KC by Hour/Firearm Used in 2018", x = "Hour", y = "Count")

#Looking at top crimes
new_cln_kcpd1 <- cln_kcpd1 %>% group_by(Offense) %>% tally()
new_cln_kcpd1[order(new_cln_kcpd1$n, decreasing=TRUE),]
new_cln_16kcpd <- cln_16kcpd %>% group_by(Offense) %>% tally()
new_cln_16kcpd[order(new_cln_16kcpd$n, decreasing=TRUE),]
new_cln_17kcpd <- cln_17kcpd %>% group_by(Offense) %>% tally()
new_cln_17kcpd[order(new_cln_17kcpd$n, decreasing=TRUE),]
new_cln_18kcpd <- cln_18kcpd %>% group_by(Offense) %>% tally()
new_cln_18kcpd[order(new_cln_18kcpd$n, decreasing=TRUE),]

#Plotting Offense Records for Month separated by year
month_df <- cln_kcpd1 %>% group_by(Month, Year) %>% tally()

ggplot(month_df, aes(x=Month, y=n, color=Year, group=Year)) +
  geom_line() +
  geom_point() +
  labs(x="Month", y="Offense Records", title="Records By Month") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Linear Models
lm(formula = Offense ~ Zip.Code, data = cln_kcpd1)
lm(formula = Offense ~ Year, data = cln_kcpd1)
lm(formula = Offense ~ Month, data = cln_kcpd1)
lm(formula = Offense ~ Firearm.Used.Flag, data = cln_kcpd1)

#Covariance
cov(cln_kcpd1$Offense, cln_kcpd1$Zip.Code, use = "complete.obs")
cov(cln_16kcpd$Offense, cln_16kcpd$Zip.Code, use = "complete.obs")
cov(cln_17kcpd$Offense, cln_17kcpd$Zip.Code, use = "complete.obs")
cov(cln_18kcpd$Offense, cln_18kcpd$Zip.Code, use = "complete.obs")

#Correlation
cor.test(cln_kcpd1$Offense, cln_kcpd1$Zip.Code, method = "spearman")
cor.test(cln_16kcpd$Offense, cln_16kcpd$Zip.Code, method = "spearman")
cor.test(cln_17kcpd$Offense, cln_17kcpd$Zip.Code, method = "spearman")
cor.test(cln_18kcpd$Offense, cln_18kcpd$Zip.Code, method = "spearman")
#Since the p-value is less thatn 0.5 we would reject our null hypothesis in this instance.

#Partial Correlation
pcor(c("Offense", "Zip.Code", "Month"), var(cln_kcpd1, use = "pairwise.complete.obs"))
pcor(c("Offense", "Zip.Code", "Month"), var(cln_16kcpd, use = "pairwise.complete.obs"))
pcor(c("Offense", "Zip.Code", "Month"), var(cln_17kcpd, use = "pairwise.complete.obs"))
pcor(c("Offense", "Zip.Code", "Month"), var(cln_18kcpd, use = "pairwise.complete.obs"))

#The partial correlation for this is positive, but still closer to 0 showing there isn't much of a relationship between our variables.
