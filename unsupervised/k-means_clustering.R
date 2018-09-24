# K-means Clustering

# Create two clusters of datapoints
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# K-means with K=2
km.out=kmeans(x,2,nstart=20)

# The cluster assignments
km.out$cluster
#  [1] 1 2 1 2 1 1 1 2 1 2 1 2 1 2 1 2 1 1 1 1 1 2 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3
# [39] 3 3 3 3 3 2 3 2 3 3 3 3

# Plot 
plot(x, col=(km.out$cluster+1), main="K-Means Clustering 
     Results with K=2", xlab="", ylab="", pch=20, cex=2)

# K-means with k=3
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
# K-means clustering with 3 clusters of sizes 17, 10, 23
# 
# Cluster means:
#         [,1]        [,2]
# 1  3.7789567 -4.56200798
# 2  2.3001545 -2.69622023
# 3 -0.3820397 -0.08740753
# 
# Clustering vector:
#  [1] 1 2 1 2 1 1 1 2 1 2 1 2 1 2 1 2 1 1 1 1 1 2 1 1 1 3 3 3 3 3 3 3 3 3 3 3 3 3
# [39] 3 3 3 3 3 2 3 2 3 3 3 3
# 
# Within cluster sum of squares by cluster:
# [1] 25.74089 19.56137 52.67700
#  (between_SS / total_SS =  79.3 %)
# 
# Available components:
# 
# [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"      

# Setting nstart at 20 results in a minimized withinSS
# Local optima are reduced when a large nstart is used
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
# [1] 97.97927
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss
# [1] 97.97927
