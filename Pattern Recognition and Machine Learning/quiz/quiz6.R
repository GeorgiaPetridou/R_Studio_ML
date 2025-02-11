
sdata = read.csv("sdata.txt")
plot(sdata, pch = 1)

X1 = c(-4,0,4)
X2 = c(10,0,10)
center = data.frame(X1, X2)

model=kmeans(sdata, centers=center)
model$centers
model$cluster

plot(sdata, col = model$cluster, pch = 1)
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

model$tot.withinss
model$betweenss

model_silhouette = silhouette(model$cluster, dist(sdata))
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])

X1 = c(-2,2,0)
X2 = c(0,0,10)
center = data.frame(X1, X2)

model=kmeans(sdata, centers=center)
model$centers
model$cluster

plot(sdata, col = model$cluster, pch = 1)
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)
 
model$tot.withinss
model$betweenss

model_silhouette = silhouette(model$cluster, dist(sdata))
plot(model_silhouette)
mean_silhouette = mean(model_silhouette[, 3])
