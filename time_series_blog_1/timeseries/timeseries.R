# fma package
library("fma")

# Unemployment benifits in Australia
data(dole)

dole.56.92 <- window(dole, 1956, c(1992, 12))
options(scipen=6) # set notation digits length
png(filename="australia_plot.png")
plot(dole.56.92, main="Unemployment benifits in Australia", ylab="benifits", xlab="Jan 1956 - July 1992")
dev.off()

dole.73.78 <- window(dole, 1973, c(1978, 12))

# Monthly total of people on unemployment benifits in Australia
# January 1973 - December 1978

# Graph
png(filename="ts_unemployment_benifits1.png")
options(scipen=6) # set notation digits length
plot(dole.73.78, main="Unemployment benifits in Australia", ylab="benifits", xlab="Jan 1973 - Dec 1978")
dev.off()
#
print("(1) A large increase in March 1974 to Feb 1975, due to an economic recession")
# March 1974 to Feb 1975
spike.74.75 <- dole.73.78[18:26]
# Rate of Increase
m.74 <- min(spike.74.75)
f.75 <- max(spike.74.75)
spike.diff <- (f.75 - m.74)
print("A Rise in Percentage")
spike.diff / m.74
print("(2) At the end of 1975 a rise due to an economic crisis")
print("(3) A long term fluctuation in the level from the begining of 1977 to 1979 as it increases")

# stationary plot
png(filename="stationary_australia.png")
plot(diff(dole.56.92), main="Unemployment benifits in Australia", ylab="", xlab="")
dev.off()

# By using the HoltWinters function we can graph a red smoothed line
png(filename="stationary_australia_HW.png")
plot(HoltWinters(diff(dole.56.92), beta=FALSE, gamma=FALSE))
dev.off()
