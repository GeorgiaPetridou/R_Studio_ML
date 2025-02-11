rm(list=ls())

# Load libraries
library(scatterplot3d)

# Create and plot data
pdata = data.frame(X = c(1,0,-1,0,-1,1), Y = c(0,1,1,-1,0,-1), Z = c(-1,-1,0,1,1,0))
row.names(pdata) <- c("x1", "x2", "x3", "x4", "x5", "x6")
s3d = scatterplot3d(pdata, color = "blue", pch = 19, scale.y = 1.5)
coords <- s3d$xyz.convert(pdata)
text(coords$x, coords$y, labels=row.names(pdata), pos=2)

# Alternate solution
# covmat = cov(pdata)
# eigenvalues = eigen(covmat)$values
# eigenvectors = eigen(covmat)$vectors
# pc = as.matrix(pdata) %*% eigenvectors[, 1:2]
# info_loss = eigenvalues[3] / (eigenvalues[1] + eigenvalues[2])

# Main solution
pca_model <- prcomp(pdata)
eigenvalues = pca_model$sdev^2
eigenvectors = pca_model$rotation
barplot(pca_model$sdev ^ 2 / sum(pca_model$sdev ^ 2))
pc <- predict(pca_model, pdata)
plot(pc, col = "blue", pch = 19)
text(pc, labels=row.names(pdata), pos=2)

