# Timeseries packages in R: xts, zoo, TTR, forecast, quantmod, tidyquant, ts

set.seed(123)
t <- seq(from = 1, to = 100, by = 1) + 10 + rnorm(100, sd = 7)

# plotting the data
print("Rplot1")
png(filename = "Rplot1.png")
plot(t)
dev.off()

# args(ts)
# function (data = NA, start = 1, end = numeric(), frequency = 1,
#     deltat = 1, ts.eps = getOption("ts.eps"), class = if (nseries >
#         1) c("mts", "ts", "matrix") else "ts", names = if (!is.null(dimnames(data))) colnames(data) else paste("Series",
#         seq(nseries)))
#
# deltat acts as a fraction of the data between a period, ie yearly data
# can be expressed as monthly data 1/12

args(plot.ts)

# creating a yearly timeseries starting in 1990
tseries <- ts(t, start = c(1990, 1), frequency = 1)
print(tseries)

# plotting the timeseries
print("Rplot2")
png(filename = "Rplot2.png")
plot(tseries)
dev.off()


# Build a matrix and plot multiple time series graphs
set.seed(123)
seq <- seq(from = 1, to = 100, by = 1) + 10
ts1 <- seq + rnorm(100, sd = 5)
ts2 <- seq + rnorm(100, sd = 12)
ts3 <- seq^2 + rnorm(100, sd = 300)
tsm <- cbind(ts1, ts2, ts3) # column bind matrix
tsm <- ts(tsm, start = c(1990, 1), frequency = 1)

# plotting the matrix
print("Rplot3")
png(filename = "Rplot3.png")
plot.ts(tsm)
dev.off()

# the arguments for plot.ts
# args(plot.ts)
# function (x, y = NULL, plot.type = c("multiple", "single"), xy.labels,
#     xy.lines, panel = lines, nc, yax.flip = FALSE, mar.multi = c(0,
#         5.1, 0, if (yax.flip) 5.1 else 2.1), oma.multi = c(6,
#         0, 5, 0), axes = TRUE, ...)
#
# plot a multi or single-panel graph using plot.type

# time series related functions
# window() start() end() frequency()

# subset of timeseries
tseries_sub <- window(tseries, start=c(1990, 1), end=c(2080, 1))
print(tseries_sub)

print("looking at the start, end, and frequency of a timeseries object")
start(tsm); end(tsm); frequency(tsm)

# timeseries require transformations using difference, log
# moving average (MA), percent change, lag, or cumulative sum


# Looking at the lag function
print("The lag function can be used to identify and remodel a timeseries graph for more accurate predictions")
print("It allows a timeseries to be offset by one")
print("Timeseries lag 1:")
tseries_lag1 <- lag(tseries,1)
head(cbind(tseries, tseries_lag1))
print("Timeseries 1990, puts the value 1989")

print("Timeseries lag 3:")
tseries_lag3 <- lag(tseries, 3)
head(cbind(tseries, tseries_lag3))
print("The timeseries has 3 previous year lag values")


print("We can lead the initial time peroid also")
tseries_lead1 <- lag(tseries, -1)
head(cbind(tseries, tseries_lead1))
print("Timeseries lead 1:")

# The difference of a time series is used to create a stationary timeseries
print("A the difference between the timeseres and future timeseries values")
tseries_diff1 <- diff(tseries, lag = 1)
tm <- cbind(tseries, tseries_diff1)
head(tm)
print("Creating a timeseries diff is crucial for doing forcasting")
print("It allows for a constant mean which makes predictions accurate")

# calculating the difference over a lag
tseries_diff2 <- diff(tseries, lag = 2)
tm <- cbind(tseries, tseries_diff2)
print("The difference over a lag 2")
head(tm)

# timeseries and stationary plots
print("Rplot4")
png(filename = "Rplot4.png")
plot(tm)
dev.off()

# for handling variances increases logarithm and box-cox transformations are useful
# create an increaseing variance graph
trend <- ts(seq(from = 10, to = 110))
cycle <- ts(sin(trend)) * 0.2 * trend
tseries_h <- trend + cycle

# use log to make the heteroskedasticity (or variance) constant
tseries_log <- log(tseries_h)
tm <- cbind(tseries_h, tseries_log)

# plotted together for comparison
print("Rplot5")
png(filename = "Rplot5.png")
plot.ts(tm)
dev.off()

# handling variance can also be done using box-cox transformations
library(forecast)

# lambda parameter is a chosen value to address heteroskedasticity
boxcox5 <- BoxCox(tseries_h, lambda = 0.5)

# using the lambda function to genereate the optimal value
lambda <- BoxCox.lambda(tseries_h)
boxcoxl <- BoxCox(tseries_h, lambda = lambda)

# heteroskedastic comparison plots
print("Rplot6")
png(filename = "Rplot6.png")
boxcoxtm <- cbind(boxcox5, boxcoxl)
plot.ts(boxcoxtm)
dev.off()

# percent change from one peroid to another
pch <- function(data, lag = 1) {
    # argument verification
    if (!is.ts(data)) stop("data must be of type ts")
    if (!is.numeric(lag)) stop("lag must be of type numeric")
    # return percent change
    data / lag(data, -lag) -1
}
print("Noting the change from period to period")
tseries_pch <- pch(tseries, lag=1)
head(cbind(tseries, tseries_pch))

# plot the timeseries difference after applying log
tseries_dlog <- ts(diff(log(tseries)), start = c(1990, 1), frequency = 1)
print("Rplot7")
png(filename = "Rplot7.png")
plot.ts(cbind(tseries, tseries_dlog))
dev.off()

# compare percentchange to dlog plots
plot.ts(cbind(pch(tseries), tseries_dlog))

# moving average helps to visualize a trend in a series with high volitility
# filter() allows general linear filtering parameters to be set up for a moving average
# a 5 peroid moving average by a weighted average of 5 observations
# a side = 1 value gives a trailing moving average, a side = 2 gives future moving averages
tseries_lf5 <- filter(tseries, filter = rep(1/5, 5), sides = 1)
print("Rplot8")
png(filename = "Rpolot8.png")
plot.ts(cbind(tseries, tseries_lf5), plot.type = 'single', col = c('black', 'red'))
dev.off()

# the SMA() function from TTR or the ma() function from the forecast package
# where n is the window over which we want to calculate the moving average
library(TTR)
tseries_ma5 <- SMA(tseries, n = 5)

# using the ma function
tseries_ma5fore <- ma(tseries, order = 5)

print("comparing linear filtering,the TTR pacakge SMA, and the forecast package moving average")
cbind(tseries_lf5, tseries_ma5, tseries_ma5fore)

