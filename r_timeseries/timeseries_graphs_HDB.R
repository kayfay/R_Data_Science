# Original source from M. Bernauer Visualize This: Patterns over time
# https://mlbernauer.github.io/R/20150308 <- visualize <- this <- patterns <- over <- time.html

library("ggplot2")

# Hotdog dataset
hd <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv")

# R base bar graph of hotdog dataset
colors <- ifelse(hd$New.record == 1, "darkred", "grey")
barplot(hd$Dogs.eaten, names.arg = hd$Year, col=colors, border=NA,
        main = "Nathan's Hot Dog Eating Contest Results, 1980 - 2010",
        xlab = "Year", ylab = "Hot dogs and buns (HDBs) eaten")

# Ggplot2 graph of hotdog datase
ggplot(hd) +
    geom_bar(aes(x = Year, y = Dogs.eaten, fill = factor(New.record)),
                stat="identity") +
    labs(title = "Nathan's Hot Dog Eating Contest Results, 1980 - 2010",
         fill = "New Record") +
    xlab("Year") +
    ylab("Hot dogs and buns (HDBs), eaten")

# Hotdog places dataset as a matrix *modified from original*
hd.top3 <- as.matrix(read.csv("http://datasets.flowingdata.com/hot-dog-places.csv",
                              sep=",", header=TRUE))

# Rename col names *modified from original*
colnames(hd.top3) <- as.character(2000:2010)

# R base stacked bar charts of hotdog dataset
barplot(hd.top3, border=NA, main="Hot Dog Eating Contest Results 2000 - 2010",
        xlab = "Year",
        ylab = "Hot dogs and buns (HDBs) eaten")
