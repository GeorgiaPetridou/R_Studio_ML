
data(Glass, package = "mlbench")
training = Glass[c(1:50, 91:146), -10]
trainingType = factor(Glass[c(1:50, 91:146), 10])
testing = Glass[51:90, -10]
testingType = factor(Glass[51:90, 10])

pca_model <- prcomp(training, center=TRUE, scale=TRUE)
eigenvalues = pca_model$sdev^2
eigenvectors = pca_model$rotation
barplot(pca_model$sdev ^ 2 / sum(pca_model$sdev ^ 2))

eigenvalues[1]/sum(eigenvalues)

sum(eigenvalues[c(5:9)])/sum(eigenvalues)

library(class)
predictions=knn(training,testing,trainingType,k=3)

Accuracy(predictions, testingType)

library(MLmetrics)
library(e1071)

Recall(testingType,predictions,"2")


pc_train = predict(pca_model, training)
pc_test = predict(pca_model,testing)
accuracy=c()

for (i in c(1:ncol(pc))) {
  
  
  pred=knn(as.data.frame(pc_train[,1:i]),as.data.frame(pc_test[,1:i]),trainingType,k=3)
  accuracy= c(accuracy, Accuracy(pred, testingType) )
  
}

print(accuracy)
which.max(accuracy)
