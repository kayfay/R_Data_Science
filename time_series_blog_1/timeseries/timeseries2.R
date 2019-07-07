# Federal Reserve Economic Data - from Quandl FRED/A791RX0Q048SBEA Retailer Sales FRED/RETAILSMSA
# Data ranges from Jan 1992 to Sep 2017
# Using the Quandl package to aquire a dataset from the Federal Reserve Economic Data
library(forecast)
library(Quandl)
feddata <- Quandl("FRED/RETAILSMSA", type="ts")

print("Rplot9")
png(filename="Rplot9.png")
plot.ts(feddata, main = "Federal Reserve Economic Data: Retail Sales", ylab = "Dollars in Thousands",
        xlab = "Years")
dev.off()

# autocorrelation function for differenced series
print("Rplot10")
png(filename="Rplot10%d.png")
acf(diff(feddata), xaxp = c(0, 75, 4), lag.max=75, main="")
pacf(diff(feddata), xaxp = c(0, 75, 4), lag.max=75, main="")
dev.off()

# autocorrelation function for an autoregressive integrated moving average
arima_fit <- auto.arima(feddata)

# inspecting
arima_fit
# The coefficients significantly different and  are close to a
# mean of zero standard deviations of the standard error


# autocorrelation function for arima_fit looks good
print("Rplot111 Rplot 112")
png(filename="Rplot11%d.png")
acf(arima_fit$residuals, main="")
pacf(arima_fit$residuals, main="")
dev.off()

# our lag values also aren't crossing the significant zones

print("Rplot121, Rplot122, Rplot123")
png(filename="Rplot12%d.png")
# plotting
plot(arima_fit$residuals, main="",  ylab = "Residuals", xlab="Time")
abline(a=0, b=0)

# histogram
hist(arima_fit$residuals, main="", ylab="Frequency", xlab="Residuals")

# qq plot
qqnorm(arima_fit$residuals, ylab="Sample Quantiles", xlab="Theoretical Quantiles", main="")
qqline(arima_fit$residuals)
dev.off()

plot(forecast(feddata, h=12),
     main = "Forecast into 2018 of Retailer Sales",
     xlab = "Time in Months",
     ylab = "Millions of Dollars",)

# Graph
png(filename="Rplot13%d.png")
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.6, mgp = c(3.5, 1, 0), cex.lab = 1.5, font.lab = 2, cex.axis = 1.5, bty = "n", las = 1)
plot(forecast(feddata, h = 24), xlab = "", ylab = "", main = "Forecast into 2018 of Retailer Sales", type="l", axes = FALSE)
axis(1) ; axis(2)
mtext("Time in Months", side = 1, line = 2.5, cex = 1.5)
mtext("Millions of Dollars", side = 2, line = 5, cex = 1.5, las = 0)
dev.off()

# lb/ub of forecast
print("ub/lb of forecast")
f.p <- forecast(feddata, h=12)
abs(tail(feddata)[6] - tail(f.p$upper)[12]) / tail(feddata)[6] # [1] 0.1330501
abs(tail(feddata)[6] - tail(f.p$lower)[12]) / tail(feddata)[6] # [1] 0.04680049
