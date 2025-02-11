rm(list=ls())

library("neuralnet")

# Create and plot the data
anndata = data.frame(X1 = c(0,0,1,1), X2  = c(0,1,0,1), Y = c(1,1,-1,-1))
Y12 = ifelse(anndata$Y > 0, 1, 2)
plot(anndata[,c("X1","X2")], col = Y12, pch = c("o","+")[Y12])

# Create a grid used to plot contours
X1 = seq(min(anndata[, 1]), max(anndata[, 1]), by = 0.001)
X2 = seq(min(anndata[, 2]), max(anndata[, 2]), by = 0.001)
mygrid = expand.grid(X1, X2)

# Plot a contour for the decision function of the ANN
model = neuralnet(Y ~ X1 + X2, anndata, hidden = 0, threshold = 0.000001)
plot(model, information = FALSE)
netres = compute(model, mygrid)
pred = ifelse(netres$net.result > 0, 1, 2)
Y = matrix(pred, length(X1), length(X2))
contour(X1, X2, Y, add = TRUE, levels = 1.5, labels = "", col = "blue")
