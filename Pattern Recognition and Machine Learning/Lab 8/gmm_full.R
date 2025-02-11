rm(list=ls())

# Import libraries
library(cluster)
library(mixtools)

# Read data
gsdata = read.csv("gsdata.txt")
target = gsdata[, 3]
gsdata = gsdata[, 1:2]

# Plot the data
plot(gsdata, col = target)

# Apply GMMs algorithm
model = mvnormalmixEM(gsdata, k = 3, epsilon = 0.1)
clusters = max.col(model$posterior)
centers = matrix(unlist(model$mu), byrow = TRUE, ncol = 2)

# Plot the data with the clusters and the density function
plot(model, which = 1)
# Alternatively use this
# plot(model$all.loglik, type = 'l')

# Plot the data with the clusters and the density function
plot(model, which = 2)
# Alternatively use this
# plot(gsdata, col = clusters)
# points(centers, col = 4, pch = "+", cex = 2)
# for (i in 1:3) ellipse(mu = model$mu[[i]], sigma = model$sigma[[i]])

# Calculate and plot silhouette
model_silhouette = silhouette(clusters, dist(gsdata))
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])

# Sort data according to cluster assignment and plot heatmap
gsdata_ord = gsdata[order(clusters),]
heatmap(as.matrix(dist(gsdata_ord)), Rowv = NA, Colv = NA, col = heat.colors(256), revC = TRUE)
