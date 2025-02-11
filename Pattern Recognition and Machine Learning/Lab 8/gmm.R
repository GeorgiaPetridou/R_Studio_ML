rm(list=ls())

# Import library
library(mixtools)

# Read data
gdata = read.csv("gdata.txt")
x = gdata[, 1]
y = gdata[, 2]

# Plot data in 1d
plot(data.frame(x, 0), ylim = c(-0.01, 0.25), col = y, xlab = "Data", ylab = "Density")
lines(density(x), col = y) # plot density on top of the data

# Or use this to better see the points
# minimal_noise = rnorm(length(x), mean = 0, sd = 0.005)
# plot(data.frame(x, minimal_noise), ylim = c(-0.01, 0.25), col = y)
# lines(density(x), col = y) # plot density on top of the data

# Expectation Maximization algorithm for GMMs where sigma = 1
# Initialize the means and the latent variables
mu = c(0, 1)
lambda = c(0.5, 0.5)
epsilon = 1e-08
log_likelihood = sum(log(lambda[1] * dnorm(x, mean = mu[1], sd = 1) + lambda[2] * dnorm(x, mean = mu[2], sd = 1)))

# Loop until convergence
repeat {

  # Expectation step
  # Find distributions given mu, lambda (and sigma)
  T1 <- dnorm(x, mean = mu[1], sd = 1)
  T2 <- dnorm(x, mean = mu[2], sd = 1)
  P1 <- lambda[1] * T1 / (lambda[1] * T1 + lambda[2] * T2)
  P2 <- lambda[2] * T2 / (lambda[1] * T1 + lambda[2] * T2) # or P2 = 1 - P1

  # Maximization step
  # Find mu, lambda (and sigma) given the distributions
  mu[1] <- sum(P1 * x) / sum(P1)
  mu[2] <- sum(P2 * x) / sum(P2)
  lambda[1] <- mean(P1)
  lambda[2] <- mean(P2)

  # Calculate the new log likelihood (to be maximized)
  new_log_likelihood = sum(log(lambda[1] * dnorm(x, mean = mu[1], sd = 1) + lambda[2] * dnorm(x, mean = mu[2], sd = 1)))
  
  # Print the current parameters and the log likelihood
  cat("mu =", mu, " lambda =", lambda, " log_likelihood =", new_log_likelihood, "\n")

  # Break if the algorithm converges
  if (new_log_likelihood - log_likelihood <= epsilon)
    break
  log_likelihood = new_log_likelihood
}

# The result of the algorithm is in the following parameters
print(mu)
print(lambda)

# Apply EM algorithm for Gaussian Mixtures
model <- normalmixEM(x, mu = c(0,1), sd.constr = c(1,1))
model$mu
model$lambda

# Plot the densities of the real and the modeled distributions
plot(model, which = 2)
lines(density(x), lty = 2, lwd = 2)
