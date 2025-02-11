rm(list=ls())

# Import machine learning libraries
library(e1071)
library(MLmetrics)
library(ROCR)

# Read data
data(HouseVotes84, package = "mlbench")
votes = na.omit(HouseVotes84)
trainingdata = votes[1:180,]
testingdata = votes[181:232,]
xtest = testingdata[,-1]
ytest = testingdata[,1]

# Apply Naive Bayes
model <- naiveBayes(Class ~ ., data = trainingdata)

# Predict on the test set and print the confusion matrix and metrics
pred = predict(model, xtest)
ConfusionMatrix(pred, ytest)
Precision(ytest, pred) # or Precision(ytest, pred, "democrat") and Precision(ytest, pred, "republican")
Recall(ytest, pred)
F1_Score(ytest, pred)

# Get the prediction probabilities and plot ROC curve
predprob = predict(model, xtest, type = "raw")
pred_obj = prediction(predprob[,1], ytest, label.ordering = c("republican", "democrat"))
ROCcurve <- performance(pred_obj, "tpr", "fpr")
plot(ROCcurve, col = "blue")
abline(0,1, col = "grey")

# Get the AUC
performance(pred_obj, "auc")
# unlist(performance(pred_obj, "auc")@y.values[1])
