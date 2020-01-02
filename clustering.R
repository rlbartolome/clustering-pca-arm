library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data

sd.data <- scale(nci.data, F, T)
data.dist <- dist(sd.data)

par(mfrow = c(1,3))

plot(hclust(data.dist), main = "Complete Linkage", labels = nci.labs) #best
plot(hclust(data.dist, method = "average"), main = "Average Linkage", labels = nci.labs)
plot(hclust(data.dist, method = "single"), main = "Single Linkage", labels = nci.labs)

hc.out <- hclust(data.dist)

hc.clusters <- cutree(hc.out, 4)

unique(nci.labs)
table(hc.clusters, nci.labs)

##kmeans

set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters) # check where they agree (check diagonal)

table(km.clusters) #quite better kasi hindi lang isa
table(hc.clusters)


## kmeans 

vars <- rep(NA, 20)

for (i in 1:20){
  vars[i] <- kmeans(sd.data, i)$tot.withinss
}
plot(vars)

## use PCA

pr.out <- prcomp(nci.data, scale = T)
screeplot(pr.out, type = "l") ## four or five?
nci.scores <- pr.out$x[ ,1:5]
nci.dist <- dist(nci.scores)
hc.out2 <- hclust(nci.dist, method = "average")

plot(hc.out2)
# try kmeans
km.out2 <- kmeans(nci.scores, 3)
table(km.out2$cluster) ## how to get clusters

## Cluster Analysis; Silhoutte Plots

library("cluster")
plot(silhouette(km.clusters, data.dist))
plot(silhouette(hc.clusters, data.dist))
