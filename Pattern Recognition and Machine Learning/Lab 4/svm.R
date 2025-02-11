rm(list=ls())

library(MLmetrics)
library(e1071)

# Load the data
alldata = read.csv("alldata.txt")
trainingdata = alldata[1:600, ]
testdata = alldata[601:800, ]

# Plot the data
plot(trainingdata[, c(1:2)], col = trainingdata$y, pch = c("o","+")[trainingdata$y])
#plot(testdata[, c(1:2)], col = testdata$y, pch = c("o","+")[testdata$y])

# Create a grid used to plot contours
X1 = seq(min(trainingdata[, 1]), max(trainingdata[, 1]), by = 0.1)
X2 = seq(min(trainingdata[, 2]), max(trainingdata[, 2]), by = 0.1)
mygrid = expand.grid(X1, X2)
colnames(mygrid) = colnames(trainingdata)[1:2]

# Plot a contour for the decision function of the SVM with gamma = 1
svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = 1)
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "gamma = 1", col = "blue")

# Plot a contour for the decision function of the SVM with gamma = 0.01
svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = 0.01)
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "gamma = 0.01", col = "red")

# Plot a contour for the decision function of the SVM with gamma = 100
svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = 100)
pred = predict(svm_model, mygrid)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "gamma = 100", col = "green")

# Set of gamma values
gammavalues = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000, 1000000)

# Calculate the training error
training_error = c()
for (gamma in gammavalues) {
  svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = gamma)
  pred = predict(svm_model, trainingdata[, c(1:2)])
  training_error = c(training_error, 1 - Accuracy(trainingdata$y, pred))
}

# Calculate the testing error
testing_error = c()
for (gamma in gammavalues) {
  svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingdata, gamma = gamma)
  pred = predict(svm_model, testdata[, c(1:2)])
  testing_error = c(testing_error, 1 - Accuracy(testdata$y, pred))
}

# Plot the training error and the testing error
plot(training_error, type = "l", col="blue", ylim = c(0, 0.5), xlab = "Gamma", ylab = "Error", xaxt = "n")
axis(1, at = 1:length(gammavalues), labels = gammavalues)
lines(testing_error, col="red")
legend("right", c("Training Error", "Testing Error"), pch = c("-","-"),  col = c("blue", "red"))

# Apply 10-fold cross validation to find the best value for gamma
k = 10
# Split in 10 folds
dsize = nrow(trainingdata)
set.seed(0); folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
for (gamma in gammavalues) {
  predictions <- data.frame()
  testsets <- data.frame()
  for(i in 1:k){
    # Select 9 out of 10 folds for training and 1 for validation
    trainingset <- trainingdata[unlist(folds[-i]),]
    validationset <- trainingdata[unlist(folds[i]),]
    # Train and apply the model
    svm_model = svm(y ~ ., kernel="radial", type="C-classification", data = trainingset, gamma = gamma)
    pred = predict(svm_model, validationset[, c(1:2)])
    # Save predictions and testsets
    predictions <- rbind(predictions, as.data.frame(pred))
    testsets <- rbind(testsets, as.data.frame(validationset[,3]))
  }
  # Calculate the new accuracy and add it to the previous ones
  accuracies = c(accuracies, Accuracy(predictions, testsets))
}
# Find the best gamma value
print(accuracies)
bestgamma = gammavalues[which.max(accuracies)]

# Plot the accuracy for each gamma value
# plot(accuracies, type = "l", col="blue", ylim = c(0.5, 1.0), xlab = "Gamma", ylab = "Accuracy", xaxt = "n", main = "Cross-Validation Accuracy")
# axis(1, at = 1:length(gammavalues), labels = gammavalues)


