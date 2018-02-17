# import the healthcare data in the current directory
df <- read.table('mydata.txt')

# open an image save format
png("file%01d.png")

# transform the dataframe elements into vectors
first <- as.vector(as.numeric(df$assess1[-9]))
second <- as.vector(as.numeric(df$assess2))
final <- as.vector(as.numeric(df$emergency))

# create boxplots
boxplot(first, ylim=c(.5,2.5))
boxplot(second, ylim=c(.5,2.5))
boxplot(final, ylim=c(.5,2.5))
boxplot(first, second, final, ylim=c(.5,2.5))

# create histograms
hist(first, breaks=2, xlab="First Assessment: bad,good")
hist(second, breaks=2, xlab="Second Assessment: low,high")
hist(final, breaks=2, xlab="Final Decision Critical Care: low,high")

# close the image capture device
dev.off()


