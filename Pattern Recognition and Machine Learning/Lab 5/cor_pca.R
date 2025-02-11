rm(list=ls())

# Load data
engdata = read.csv("engdata.txt")
Location = engdata[, 5]
engdata = engdata[, 1:4]

# Plot data
plot(engdata, col = Location, pch = c("o", "+")[Location])

# Calculate correlation
cor(engdata)

# Calculate PCA
pca_model <- prcomp(engdata, center = TRUE, scale = TRUE)
eigenvectors = pca_model$rotation
eigenvalues = pca_model$sdev^2

# Plot the variance percentage for each component
barplot(eigenvalues / sum(eigenvalues))

# Apply the transformation and plot the transformed data
engdata_pc <- as.data.frame(predict(pca_model, engdata)[, 1:2])
plot(engdata_pc, col = Location, pch = c("o", "+")[Location])

# Reconstruct the original data
engdata_pc[, 3:4] <- 0
engdata_rec = data.frame(t(t(as.matrix(engdata_pc) %*% t(pca_model$rotation)) * pca_model$scale + pca_model$center))
plot(engdata_rec, col = Location, pch = c("o", "+")[Location])

# Find the amount of information lost
info_loss = (eigenvalues[3] + eigenvalues[4]) / sum(eigenvalues)
