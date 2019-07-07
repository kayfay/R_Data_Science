library(forecast)

# read in the table
per_cap <- read.table("transforms/pencap.csv", header=TRUE, sep=",", row.names=1)

# set the row/group names
names(per_cap)  <- c("2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992")

# create an descending version of the data
yr_dsend_per_cap <- replace(per_cap, TRUE, rev(per_cap))
names(yr_dsend_per_cap) <- rev(names(yr_dsend_per_cap))

# Break up the data into each catagory and give them easy to reference labels
total <- t(per_cap)[1:23]
motor <- t(per_cap)[24:46]
furniture <- t(per_cap)[47:69]
electronics <- t(per_cap)[70:92]
building <- t(per_cap)[93:115]
food <- t(per_cap)[116:138]
health <- t(per_cap)[139:161]
gas <- t(per_cap)[162:184]
clothing <- t(per_cap)[185:207]
hobby <- t(per_cap)[208:230]
gen_goods <- t(per_cap)[231:253]
misc <- t(per_cap)[254:276]
nonstore <- t(per_cap)[277:299]
resturants <- t(per_cap)[300:322]

# Create a second data frame from the categories
percap <- as.data.frame(cbind(total, motor, furniture, electronics, building, food, health, gas, clothing, hobby, gen_goods, misc, nonstore, resturants, make.col.names=TRUE), nrow=23)
percap <- percap[-15]
attributes(percap)$row.names <- c("2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992")

# summary of per_cap
print("Summary of Estimated U.S. Per Capita Retail Sales by Year of Selected Kind of Business: 1992 Through 2014")
summary(per_capi[2:14,]) # by year without the total column
print("Looking at the quantiles of yearly sales across all catagories for each year")
print("we get an idea about the ")

# summary of percap
print("Summary of Estimated U.S. Per Capita Retail Sales by Selected Kind of Business: 1992 Through 2014")
summary(percap) # by catagory
print("Looking at the yearly medians and means shows a steady increase yearly")

# barplot and write to file
png(filename="est_barplot.png")
barplot(percap$total, main="Summary of Estimated U.S. Per Capita Retail Sales", col=cm.colors(14, alpha=.4), names=row.names(percap))
dev.off()

# boxplot and write to file
png(filename="est_boxplot.png")
boxplot(percap, border=rainbow(5))
boxplot(per_cap, border=rainbow(5))
dev.off()

# Create a few timeseries to examine
tseries <- ts(t(yr_dsend_per_cap), start=1992)
ts.total <- ts(rev(total), start=1992)
ts.motor <- ts(rev(motor), start=1992)
ts.food <- ts(rev(food), start=1992)
ts.health <- ts(rev(health), start=1992)
ts.clothing <- ts(rev(clothing), start=1992)

# Graph
png(filename="tseries1.png", width=1600, height=1200)
plot(tseries[,1:10], main="US Per Capita by Stores type", xlab="Years of US Per Captia", cex.lab=0.6)
dev.off()

# Comparing
ts.tot <- tseries[,1]
ts.tot.bx <- BoxCox(tseries[,1], lambda = BoxCox.lambda(tseries[,1]))
ts.tot.lg <- log(tseries[,1])
print("tseries3 plot")
png(filename="tseries3.png")
plot.ts(cbind(TimeSeries=ts.tot, BoxCox=ts.tot.bx, Log=ts.tot.lg), main="US Percap by Store Totals", xlab="Years: 1992 - 2014")
dev.off()

# Generating a forecast
ts.percap <- ts(yr_dsend_per_cap)
ts.percap.tot <- ts.percap[1,-22:-23]
arima.percap.tot <- auto.arima(ts.percap.tot)


# Graph
png(filename="percap_forecast.png")
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.3, mgp = c(3.5, 1, 0), cex.lab = 1.5, font.lab = 2, cex.axis = 1.5, bty = "n", las = 1)
plot(forecast(arima.percap.tot, h=2), xlab = "", ylab = "", main = "2015 Predicted Total Retailer Sales", type="p", axes = FALSE)
axis(1) ; axis(2)
mtext("Number of Years Since 1992", side = 1, line = 2.5, cex = 1.5)
mtext("Thousands of Dollars", side = 2, line = 4.5, cex = 1.5, las = 0)
dev.off()

# Diff Graph
png(filename="percap_diff.png")
op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.3, mgp = c(3.5, 1, 0), cex.lab = 1.5, font.lab = 2, cex.axis = 1.5, bty = "n", las = 1)
plot(diff(ts.total), xlab = "", ylab = "", main = "", type="b", axes = FALSE)
axis(1) ; axis(2)
mtext("Number of Years Since 1992", side = 1, line = 2.5, cex = 1.5)
dev.off()

