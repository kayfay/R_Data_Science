library("ggplot2")
library("gridExtra")

# Function for year by passed in date
year <- function(x) as.POSIXlt(x)$year + 1900

# create a year column
economics$year <- year(economics$date)

# ratio of the population unemployed
upop.rat = economics$unemploy / economics$pop

# plots for unemployed and ratio of unemployed by date
plot1 <- qplot(date, upop.rat, data = economics, geom = "line") +
    xlab("Date") +
    ylab("Unemployment by Population\n as Ratio")

plot2 <- qplot(date, uempmed, data = economics, geom = "line") +
    xlab("Date") +
    ylab("Unemploymed")


grid.arrange(plot1, plot2, ncol = 1)

# plot with color scaling
plot3 <- qplot(upop.rat, uempmed, data = economics,
               geom = c("point", "path"),
               color = year) +
    ylab("Unemployed") +
    xlab("Unemployment by Population\n as Ratio")


grid.arrange(plot1, plot2, plot3, ncol = 1)

