

# Read data from disk
setwd("...")
weather = read.csv("weather.txt")

Entropy_Sunny

#Humidity
absfreq=table(weather[which(weather$Outlook=="Sunny"),c(3,4)])
freq=prop.table(absfreq,1)
freqsum=rowSums(prop.table(absfreq))

Entropy_High = - freq["High", "No"] * log2(freq["High", "No"]) - freq["High", "Yes"] * log2(freq["High", "Yes"])
Entropy_Low = - freq["Low", "No"] * log2(freq["Low", "No"]) - freq["Low", "Yes"] * log2(freq["Low", "Yes"])

Gain_Humidity=Entropy_Sunny- freqsum["High"]*Entropy_High-freqsum["Low"]*Entropy_Low

#Temperature
absfreq=table(weather[which(weather$Outlook=="Sunny"),c(2,4)])
freq=prop.table(absfreq,1)
freqsum=rowSums(prop.table(absfreq))

Entropy_Hot = - freq["Hot", "No"] * log2(freq["Hot", "No"]) - freq["Hot", "Yes"] * log2(freq["Hot", "Yes"])
Entropy_Cool = - freq["Cool", "No"] * log2(freq["Cool", "No"]) - freq["Cool", "Yes"] * log2(freq["Cool", "Yes"])
GAIN_Temperature = Entropy_Sunny - freqsum["Hot"] * Entropy_Hot - freqsum["Cool"] * Entropy_Cool
