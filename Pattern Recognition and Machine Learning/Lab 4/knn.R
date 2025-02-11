rm(list=ls())

# Read data from disk
setwd("...")
knndata = read.csv("knndata.txt")

# Import machine learning libraries
library(class)

# Split data
X_train = knndata[,c("X1","X2")]
Y_train = knndata$Y
plot(X_train, col = Y_train, pch = c("o","+")[Y_train])
X_test = matrix(c(0.7, 0.6), ncol = 2)

# Apply kNN with k = 1 for (0.7, 0.4)
knn(X_train, c(0.7, 0.4), Y_train, k = 1, prob = TRUE)

# Apply kNN with k = 5 for (0.7, 0.4)
knn(X_train, c(0.7, 0.4), Y_train, k = 5, prob = TRUE)

# Apply kNN with k = 1 for (0.7, 0.6)
knn(X_train, c(0.7, 0.6), Y_train, k = 1, prob = TRUE)

