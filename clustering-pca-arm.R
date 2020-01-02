## Clustering
library(cluster)
uni <- read.csv("data1_Babe.csv")

seed <- 99
set.seed(seed)
sd.data <- scale(uni, F, T)
km.out <- kmeans(sd.data, 8)
table(km.out$cluster)


vars <- rep(NA, 20)
for (i in 1:20){
  set.seed(seed)
  vars[i] <- kmeans(sd.data, i)$tot.withinss
}
plot(vars)

set.seed(seed)
km.out <- kmeans(sd.data, 8)
table(km.out$cluster)

for (i in 3:20){
  set.seed(seed)
  km.out_n <- kmeans(sd.data, i)
  table(km.out_n$cluster) %>% print()
}

#always one outlier

par(mfrow = c(1,3))
for (n in c(6,7,8)){
  set.seed(seed)
  km.out <- kmeans(sd.data, n)
  data.dist <- dist(sd.data)
  plot(silhouette(km.out$cluster, data.dist))
}

## seven clusters

sd.data <- scale(uni)
data.dist <- dist(sd.data)
set.seed(seed)
plot(hclust(data.dist), main = "Complete Linkage", cex = 0.3)

#too one sided!
par(mfrow = c(1,2))
plot(hclust(data.dist, method = "average"), main = "Average Linkage", cex = 0.3)
plot(hclust(data.dist, method = "single"), main = "Single Linkage",  cex = 0.3)

for (i in 3:15){
  set.seed(seed)
  hc.out <- hclust(data.dist)
  hc.clusters <- cutree(hc.out, i)
  table(hc.clusters) %>% print()
}

## Part 2:

library(arules)
library(arulesViz)
library(tidyverse)
fin <- read.transactions("data2_Babe.csv", format = 'basket', sep = ',')
itemFrequencyPlot(fin, topN=10, type="absolute", main="Item Frequency")

summary(fin)

f <- apriori(fin, parameter = list(supp=0.05, conf=0.75, minlen = 2))
inspect(sort(f, by = 'lift')[1:10])
inspect(sort(f, by = 'confidence')[1:5])

f_n <- apriori(fin, parameter = list(supp=0.001, conf=0.75, minlen = 2))

not_philhealth <- f_n %>% subset(!rhs %in% "philhealth") %>% sort(by="lift", decreasing = F) 
inspect(sort(not_philhealth, by = 'lift')[1:5])

not_philhealth_conf <- f_n %>% subset(!rhs %in% "philhealth") %>% sort(by="confidence", decreasing = F) 
inspect(sort(not_philhealth_conf, by = 'confidence')[1:5])

#saavings
not_philhealth <- f_n %>% subset(rhs %in% "microfinance loan") %>% sort(by="confidence", decreasing = F) 
inspect(sort(not_philhealth, by = 'confidence')[1:5])




