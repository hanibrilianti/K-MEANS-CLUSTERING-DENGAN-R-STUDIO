library(readxl)

d <- read_excel("data.xlsx")

# Convert categorical column to numeric
library(dplyr)
encoded_d <- d %>%
  mutate(across(where(is.character), ~as.numeric(factor(.))))
View(encoded_d)

#determine the optimal number of clusters
library(cluster)
library(factoextra)
library(NbClust)
#Elbow Method
fviz_nbclust(encoded_d, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2) +
  labs(title = "Elbow Method")
# Silhouette analysis
sil <- fviz_nbclust(encoded_d, kmeans, method = "silhouette")
plot(sil, main = "Silhouette Analysis")

#K-means Clustering with 3 center
clust <- kmeans(encoded_d, 2)
fviz_cluster(clust, encoded_d)    #visualize the clusters formed
clust$centers
