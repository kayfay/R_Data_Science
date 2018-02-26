# install and load plyr
if(!require(plyr)){install.packages("plyr")}
library(plyr)

# Read in the file
# x <- read.table(file.choose(), header=T, sep=",")
x <- read.table(file="dataset.txt", header=T, sep=",") # dataset in current directory

# Split, Add, and Combine with a Grades column
(y = ddply(x, "Sex", transform, Grade.Average=mean(Grade)))

# write the file out in csv format
write.table(y, "Sorted_Average.csv", sep=",")

# Subset containing i and I
sub.x <- subset(x, grep[Ii], x$Name)

# write the file out in csv format
write.table(sub.x, "Data_Subset.csv", sep=",")
