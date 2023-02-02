library(ggplot2)
library(GGally)
# library(DMwR2)
library(tidyverse)
library(dplyr)
library(cluster)
library(factoextra)


prod <- read.csv("customer_clusters.csv", nrows = 2000)

ggpairs(prod[, which(names(prod) != "CUSTOMER_SK")], upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Customers before removing outlier ")

boxplot(prod[, which(names(prod) != "CUSTOMER_SK")],xlab  = "Finding outliers")

prod.clean <- prod[prod$CUSTOMER_SK != 1,]

ggpairs(prod.clean[, which(names(prod) != "CUSTOMER_SK")], upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Customers after removing outlier ")

prod.scale <- scale(prod.clean[-1])

pca_cust = prcomp(prod.clean[, which(names(prod) != "CUSTOMER_SK")], center = TRUE, scale = TRUE)

cust_transform = as.data.frame(-pca_cust$x[,1:2])

fviz_nbclust(cust_transform, kmeans, method = 'wss')

# fviz_nbclust(cust_transform, kmeans, method = 'silhouette')
#
# fviz_nbclust(cust_transform, kmeans, method = 'gap_stat')

k = 5

kmeans_custucts = kmeans(cust_transform, centers = k, nstart = 50)

fviz_cluster(kmeans_custucts, data = cust_transform)

prod.clean$Cluster <- kmeans_custucts$cluster


write.csv(prod.clean,"Clustered_Customers.csv",row.names = FALSE)