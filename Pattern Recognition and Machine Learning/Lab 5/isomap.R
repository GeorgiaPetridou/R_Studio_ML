rm(list=ls())

# Load libraries
library(vegan)
library(scatterplot3d)

# Read data
srdata = read.csv("srdata.txt")
scatterplot3d(srdata, angle = 88, scale.y = 5)

# Apply Isomap
srdata_dist <- dist(srdata)
isom <- isomap(srdata_dist, ndim=2, k = 4)
srdata_2d <- isom$points

# Plot the swiss roll and plot also the 2d representation 
colors = srdata_2d[,1] - min(srdata_2d[,1]) + 1
scatterplot3d(srdata, angle = 88, scale.y = 5, color = colors)
x11(); plot(srdata_2d, col = colors)
