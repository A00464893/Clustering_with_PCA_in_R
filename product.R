library(ggplot2)
library(GGally)
# library(DMwR2)
library(tidyverse)
library(dplyr)
library(cluster)
library(factoextra)



prod <- read.csv("productcluseter.csv", nrows = 2000)


ggpairs(prod[, which(names(prod) != "ITEM_SK")], upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Products before outlier removal")

boxplot(prod[, which(names(prod) != "ITEM_SK")], xlab  = "Finding outliers" )

prod.clean <- prod[prod$ITEM_SK != 11740941, ]

ggpairs(prod[, which(names(prod.clean) != "ITEM_SK")], upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Products after outlier removal")

prod.scale = scale(prod.clean[-1])

pca_prod = prcomp(prod.clean[, which(names(prod) != "ITEM_SK")], center = TRUE, scale = TRUE)

prod_transform = as.data.frame(-pca_prod$x[,1:2])

fviz_nbclust(prod_transform, kmeans, method = 'wss')

# fviz_nbclust(prod_transform, kmeans, method = 'silhouette')

# fviz_nbclust(prod_transform, kmeans, method = 'gap_stat')

k = 5

kmeans_products = kmeans(prod_transform, centers = k, nstart = 50)

fviz_cluster(kmeans_products, data = prod_transform)

prod.clean$Cluster <- kmeans_products$cluster


write.csv(prod.clean,"Clustered_Products.csv",row.names = FALSE)