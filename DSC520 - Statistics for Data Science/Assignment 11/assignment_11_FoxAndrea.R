library(devtools)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)

setwd("C:/Users/Andrea Fox/OneDrive/Documents/R/DSC520 Statistics Using R")
#Import the raw data and create initial dataframe
kcpd.18 <- read.csv("KCPD_Crime_Data_2018.csv", header = TRUE)
summary(kcpd.18)
names(kcpd.18)
install.packages("lubridate")
library(lubridate)
kcpd.18$Reported_Date = as.POSIXct("01/01/2018")
format(kcpd.18$Reported_Date,"%m")
ggplot(kcpd.18, aes(x = Offense, y = kcpd.18$Reported_Date, colour = Firearm.Used.Flag)) +
  geom_point()
