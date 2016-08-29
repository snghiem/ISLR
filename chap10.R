?USArrests
states = row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
 
pr.out = prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
# these are the principal component score vectors
summary(pr.out$x)
biplot(pr.out, scale=0, cex=.7)

#change the directions of the loading to the opposite without changing the correlation
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale=0, cex=.7)

pr.out$sdev
pvar = pr.out$sdev^2 #variation

# compute the proportion of variance explained by each principle component 
pv = pvar/sum(pvar)
pv
plot(pv, xlab="Principle Component", ylab="Proportion of Variance Explained", ylim = c(0,1), type='b')
plot(cumsum(pv), xlab="Principle Component", ylab="Cumulative Proportion of Variance Explained",
     ylim = c(0,1), type="b")

##################
set.seed(2)
x=matrix(rnorm(2*50), ncol=2)
x[1:25,1]= x[1:25,1]+3
x[1:25,2]= x[1:25,2]-4

# Perform K-means clustering with K=2
km.out = kmeans(x,centers=2, nstart=20)
km.out$cluster
plot(x, col=(km.out$cluster+2), main="K-means Clustering with K=2")

set.seed(4)
km.out = kmeans(x, centers=3, nstart=20)
km.out

#hierarchical clustering
hc.complete = hclust(dist(x), method="complete")
hc.average = hclust(dist(x), method="average")
hc.single = hclust(dist(x), method = "single")
par(mfrow=c(1,3))
plot(hc.complete, main="Complete")
plot(hc.average, main="Average")
plot(hc.single, main="Single")

cutree(hc.complete, 2)
cutree(hc.average,2)
cutree(hc.single,2)
cutree(hc.single, 4)

#scale the variables before using hierarchical clustering
newx = scale(x)
newhc.complete = hclust(dist(newx), method="complete")
plot(newhc.complete)
