# Using unsupervised techniques on Genomic Data
library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data

# 6830 genes on 64 cancer cell lines
dim(nci.data)

# Examining the cancer types
nci.labs[1:4]
table(nci.labs)

# PCA
pr.out = prcomp(nci.data, scale=TRUE)

# Visualization function
Cols=function(vec) {
   cols=rainbow(length(unique(vec)))
   return(cols[as.numeric(as.factor(vec))])
  }

# Plot the first few PC score vectors
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,
    xlab="Z1", ylab="Z2"
     )
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs),pch=19,
     xlab="Z1",ylab="Z3")

# Summary for PVE
summary(pr.out)

# Plot
plot(pr.out)

# Cumulative PVE
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=(c(1,2)))
plot(pve, type="o", ylab="PVE", xlab="Principal Component",
     col="blue")

plot(cumsum(pve), type="o", ylab="Cumulative PVE", 
     xlab="Principal Component", col="brown3")

# The first seven princiapl components explain 
# around 40% variance with little variance after


# Clustering on NCI60

# Standardize
sd.data=scale(nci.data)

# Plot Hierarchical Dendrograms
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs,
     main="Complete Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs,
     main="Average Linkage", xlab="", sub="", ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,
     main="Single Linkage", xlab="", sub="", ylab="")

# Cut at a high with four clusters
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)

# Note leuemia cells fall in cluster 3
# breast cancer cells are spread out over 3 clusters
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139,col="red")

# Printing summary of hc.out
hc.out

# Compare to k-means
set.seed(2)
km.out=kmeans(sd.data,4,nstart=20)
km.clusters=km.out$cluster
# Similar clustering 
table(km.clusters,hc.clusters)

# With the first few principal component scores
hc.out = hclust(dist(pr.out$x[,1:5]))
plot(hc.out,labels=nci.labs,
     main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4),nci.labs)
