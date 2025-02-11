rm(list=ls())

# Read data from disk
setwd("...")
traffic = read.csv("traffic.txt")

# Import machine learning libraries
library(e1071)

# Apply Naive Bayes
model <- naiveBayes(HighTraffic ~ ., data = traffic)

# Predict for a vacation day with hot weather
trvalue <- data.frame(Weather = factor("Hot", levels(traffic$Weather)),  Day = factor("Vacation", levels(traffic$Day)))
predict(model, trvalue)
predict(model, trvalue, type = "raw")

# Apply Naive Bayes with Laplace Smoothing
model <- naiveBayes(HighTraffic ~ ., data = traffic, laplace = 1)

# Predict for a weekend day with hot weather
trvalue <- data.frame(Weather = factor("Hot", levels(traffic$Weather)),  Day = factor("Weekend", levels(traffic$Day)))
predict(model, trvalue)
predict(model, trvalue, type = "raw")

