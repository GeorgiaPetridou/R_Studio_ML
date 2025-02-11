rm(list=ls())

# Import library
library(mixtools)

# Read data
icdata = read.csv("icdata.txt")
x = icdata[, 1]
y = icdata[, 2]

# Plot data in 1d
plot(data.frame(x, 0), ylim = c(-0.01, 0.1))
lines(density(x)) # plot density on top of the data

# Determine number of gaussian mixtures using AIC and BIC
AIC = c()
BIC = c()
par(mfrow = c(2, 2))
for (k in 2:5) {
  model <- normalmixEM(x, k = k, epsilon = 0.0001)
  plot(model, which = 2, main2 = paste("Density Curves (k = ", k, ")", sep = ""))
  numparams = length(model$mu) + length(model$stdev) + length(model$lambda)
  AIC[k] = 2 * numparams - 2 * model$loglik
  BIC[k] = log(length(x)) * numparams - 2 * model$loglik
}
par(mfrow = c(1, 1))

# Plot AIC
plot(AIC[2:5], type = 'l', xaxt = "n", main = "AIC", ylab = "AIC")
axis(1, at = 1:4, labels = 2:5)

# Plot BIC
plot(BIC[2:5], type = 'l', xaxt = "n", main = "BIC", ylab = "BIC")
axis(1, at = 1:4, labels = 2:5)
