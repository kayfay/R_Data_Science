# Perform PCA on USArrests dataset
states = row.names(USArrests)
states
names(USArrests)

apply(USArrests, 2, mean)

apply(USArrests, 2, var)

# Performing PCA
pr.out = prcomp(USArrests, scale=TRUE)

names(pr.out)

# Scaling variables prior to reduction with PCA
pr.out$center

pr.out$scale

# princial components loadings/scores
pr.out$rotation

# The PCA score vectors
dim(pr.out$x)

# Plot components 1 and 2
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

# Standard deviations of each princpal component
pr.out$sdev
# [1] 1.5748783 0.9948694 0.5971291 0.4164494

# Variance explained by each princial component
pr.var = pr.out$sdev^2
pr.var

# Proportion of variance explained
pve = pr.var/sum(pr.var)
pve

# Plotting PVE by each component and the cumulative PVE
plot(pve, xlab="Principal Component", ylab="Proportion of 
     Variance Explained", ylim=c(0,1), type='b')
# Cumulative PVE
plot(cumsum(pve), xlab="Principal Component", ylab="
     Cumulative Proportion of Variance Explained", ylim=c(0,1),
     type='b')
