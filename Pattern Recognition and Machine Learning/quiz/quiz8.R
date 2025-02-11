kmdata = read.csv("kmdata.txt")
x = kmdata[, 1:2]
y = kmdata[, 3]

windows()
plot(x, pch=1)

windows()
plot(x, col = y+1,pch=1)

model=kmeans(x, 3)
model$centers
model$cluster
Accuracy(y,model$cluster)


windows()
plot(x, col = model$cluster, pch = 1)
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)


model = mvnormalmixEM(x, k = 3, epsilon = 0.1)
clusters = max.col(model$posterior)
centers = matrix(unlist(model$mu), byrow = TRUE, ncol = 2)
Accuracy(y,clusters)


windows()
plot(model, ylim = c(8, 16), xlim=c(15,45), which = 2)
