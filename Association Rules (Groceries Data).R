library(arules)
library(arulesViz)

groceries_actual <- read.csv("groceries.csv", sep =',')
groceries <- read.transactions("groceries.csv", sep =',')

image(groceries[1:100])

groceryrules <- apriori(groceries, parameter = list(support = 0.006, confidence = 0.25, minlen = 2))

inspect(sort(groceryrules, by = 'lift')[1:10])

##subssetting
groceryrules %>% subset(rhs %in% "whole milk") %>% sort(by="lift", decreasing = F) %>% inspect() %>% head(10)
groceryrules %>% subset(rhs %pin% "milk") %>% inspect(10)

itemFrequencyPlot(groceries, topN = 20)
itemFrequencyPlot(groceries, support = 0.6)


load("marketing_sparse.Rdata") 
inspect(as(marketing, as("transactions")))

marketing_a <- marketing %>% apriori(parameter = list(support =
                                        0.07, confidence = 0.75, minlen = 2))

  

