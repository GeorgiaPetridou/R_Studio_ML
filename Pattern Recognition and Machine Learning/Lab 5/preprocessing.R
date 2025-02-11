rm(list=ls())

# Load data
engdata = read.csv("engdata.txt")
pdata = engdata[, 1:2]

# Remove duplicates
pdata = unique(pdata)

# Optionally set the graphs to split in half
# par(mfrow=c(1,2))
# Revert with par(mfrow=c(1,1))

# Center and scale the data 
plot(pdata)
transformed <- scale(pdata, scale = TRUE)
plot(transformed)

# New data can be scaled using the following command
# scale(pdata, center = attr(transformed, "scaled:center"), scale = attr(transformed, "scaled:scale"))

# Normalize data
# normalized = as.data.frame(lapply(pdata, function(x) (x - min(x))/(max(x) - min(x))))

# Sample the data and check if their structure remains the same
plot(pdata)
set.seed(0); sampdata = pdata[sample(nrow(pdata), 250, replace = TRUE),]
plot(sampdata)

# Discretize the data
discAge = cut(pdata$Age, seq(0,80,10))
plot(discAge)
discSalary = cut(pdata$Salary, seq(0,4000,400), dig.lab = 4)
plot(discSalary, las=2)
