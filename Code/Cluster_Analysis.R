#### Clustering Analysis ####

library(tidyverse)
library(mclust)

myct <- read.csv("Outputs/Combined.csv")
glimpse(myct)

M <- myct_tidy$M
M <- as.data.frame(M)
wss <- (nrow(M)-1)*sum(apply(M,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(M, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Cluster analysis with two groups

fit <- kmeans(M, 2)

aggregate(myct_tidy, by = list(fit$cluster), FUN = mean)

clustering <- data.frame(myct_tidy, fit$cluster)

ggplot(clustering, aes(x = fit.cluster, fill = sciname)) +
  geom_bar()

## Cluster analysis with four groups

fit <- kmeans(M, 4)

aggregate(myct_tidy, by = list(fit$cluster), FUN = mean)

clustering <- data.frame(myct_tidy, fit$cluster)

ggplot(clustering, aes(x = fit.cluster, fill = sciname)) +
  geom_bar()

## Find number of clusters without predermined number


