# Hierarchial Clustering
# Plot a hierarchical clustering dendrogram
# with complete, single, and average linkage
# clustering with Euclidean distance
# for dissimilarity measures

# Create two clusters of datapoints
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# Cluster observations using complete linkage
# average and single
# dist() for a 50 by 50 )
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

# Plot the three dendrograms
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="",
     cex=.9)
plot(hc.average,main="Average Linkage", xlab="", sub="",
     cex=.9)
plot(hc.(single,main="Single Linkage", xlab="", sub="",
         cex=.9)

# Cluster labels with a given cut of the dendrogram
cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,4)

# Scaling prior to hierarchical clustering
xsc = scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical
     Clustering with Scaled Features")

# Correlation based distance using as.dist()
# for square symmetric matrix as a distance matrix
# Expanding data to three features for correlation
x=matrix(rnorm(30*3), ncol=3)
dd = as.dist(1-cor(t(x)))
plot(hsclust(dd, method="complete"), main="Complete Linkage
     with Correlation-Based Distance", xlab="", sub="")
