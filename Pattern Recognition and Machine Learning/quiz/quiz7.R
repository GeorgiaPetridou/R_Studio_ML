dcdata = read.csv("dcdata.txt")
target = dcdata[, 3]
dcdata = dcdata[, 1:2]
plot(dcdata,col=target+1, pch = 1)


d = dist(dcdata)
sil=c()

# Perform hierarchical clustering with single link
hc_single = hclust(d, method = "single")
#plot(hc_single)

# Perform hierarchical clustering with complete link
hc_complete = hclust(d, method = "complete")
#plot(hc_complete)

# Split data into two clusters using single link and plot them
clusters = cutree(hc_single, k = 2)
model_silhouette = silhouette(clusters, d)
sil= c(sil, mean(model_silhouette[, 3]))

Accuracy(target,clusters)
      
windows()
plot(dcdata, col = clusters+1, pch = 1, main = "Single Linkage")


# Split data into two clusters using complete link and plot them
clusters = cutree(hc_complete, k = 2)
model_silhouette = silhouette(clusters, d)
sil= c(sil, mean(model_silhouette[, 3]))

Accuracy(target,clusters)

windows()
plot(dcdata, col = clusters+1, pch = 1, main = "Complete Linkage")

# Apply DBSCAN with eps = i and minPts = 5

library("dbscan")

for (i in c(0.75,1,1.25,1.5)) {
model = dbscan(dcdata, eps = i, minPts = 2)
clusters = model$cluster
model_silhouette = silhouette(clusters, d)
sil= c(sil, mean(model_silhouette[, 3]))

windows()
plot(dcdata, col = clusters + 1, pch = ifelse(model$cluster, 1, 4), main = "DBSCAN")
}


# Apply k-Means clustering
model = kmeans(dcdata, 2 )
model$centers
clusters=model$cluster
model_silhouette = silhouette(clusters, d)
sil= c(sil, mean(model_silhouette[, 3]))

windows()
plot(dcdata, col = model$cluster, pch = 1)
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

print(sil)
which.max(sil)