# data 'USJudgeRatings' https://vincentarelbundock.github.io/Rdatasets/datasets.html
# Lawyers' Ratings of State Judges in the US Superior Court
# Rows 43 Columns 12

# Import data
web.data <- url('https://raw.github.com/vincentarelbundock/Rdatasets/master/csv/datasets/USJudgeRatings.csv')
lr  <- read.csv(web.data, header=T)

#### Cleaning and transforming the dataset ####

# Loop over names and remove first initials
a <- c()
for (i in 1:nrow(lr)) {
        a[i] <- apply(lr[1], 2, FUN=strsplit, split=",")[[1]][[i]][1]
    }

# Rename 33rd judge
a[33] <- paste("D ", a[33])

# Rename Judges column and remove first column
attributes(lr)$row.names <- a
lr <- lr[,-1]

# Add totals and averages for each of the rows
lr.x <- lr
lr$MEAN <-  addmargins(as.matrix(lr.x), 2, FUN=mean)[,13]
lr$MEAN <- round(lr$MEAN, digits = 1)
lr$TOTAL <- addmargins(as.matrix(lr.x), 2)[,13]

# Manipulating order
lr.b <- lr$TOTAL[order(lr$TOTAL)] # order bars
n.b <- attributes(lr)$row.names[order(lr$TOTAL)] # order bar names

#### The chart code ####

# Create a barplot with the standard R package
barplot(lr.b, names.arg=n.b, horiz=T, space=1.5,
        main="Total of ratings of Judges",
        ylab = "", cex.names=.7, las=2)

# A stacked version of the bar chart
# lr.M <-  t(as.matrix(lr[-14]))
# barplot(lr.M, names.arg=a, horiz=T, space=1.5, main="Total of ratings of Judges", cex.names=.7, las=2)

library(lattice)
# lattice sorts by alphabet in names so we create a factorized scale with reorder
n.c <- reorder(n.b, lr.b)
barchart(n.c ~ lr.b, xlim=c(0, max(lr.b)),
         ylab = "", main = "Total of ratings of judges",)

library(ggplot2)
ggplot(lr$TOTALS, aes(n.c, lr.b)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("Total rating in points") +
    ggtitle("Total ratings of judges")

