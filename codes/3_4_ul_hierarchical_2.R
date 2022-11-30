# Below a hierarchical clustering agnes with euclidean complete method is used,
# but it can be easily replaced by
# single, average or ward method (see docs of ?agnes and ?hclust)

# Clean enviroment
rm(list=ls())

# Set the seed
# set.seed(12345678)


##############################
##############################
######   IRIS DATASET   ######
##############################
##############################

K = 3
library(cluster)
res.agnes <- agnes(x = iris[,1:4], # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "complete" # Linkage method
)


# Plot the dendogram (from factoextra package)
library(factoextra)
fviz_dend(res.agnes,
          cex = 0.5, # Label size
          color_labels_by_k = TRUE,  # color labels by groups
          k = K, # Cut in four groups
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          # or use palette instead of k_colors
          # palette = "jco" # Color 
          ggtheme = theme_gray()     # Change theme (theme_gray(), theme_bw(), theme_minimal(), theme_classic(), theme_void())
)

# Clusters assignments cutting for just K clusters
cutTree <- cutree(res.agnes, k = K)

# Let’s make a scatterplot.
plot(iris$Petal.Length, iris$Petal.Width, col=cutTree, pch=19)
# why a red point? --> rember that this is just a projection on 2 variables (Petal.Length, Petal.Width)
legend("topleft", fill = unique(cutTree), legend = c(unique(cutTree)), cex = 1.2)


# We can compare the predicted clusters with the original data.
table(cutTree, iris$Species)


################################
################################
####   US_ARRESTS DATASET   ####
################################
################################

# list all available datasets in all loaded libraries
# data()

# Dataset USArrests: Violent Crime Rates By US State
# This data set contains statistics, in arrests per 100,000 residents 
# for assault, murder, and rape in each of the 50 US states in 1973. 
# Also given is the percent of the population living in urban areas.
head(USArrests)


############################################
# AGNES using clustering package
############################################
K = 4

# Launch hierarchical clustering (from cluster package)
library(cluster)
res.agnes <- agnes(x = USArrests, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "complete" # Linkage method
)

# Plot the dendogram (from factoextra package)
library(factoextra)
fviz_dend(res.agnes,
          cex = 0.85, # Label size
          color_labels_by_k = TRUE,  # color labels by groups
          k = K, # Cut in four groups
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          # or use palette instead of k_colors
          # palette = "jco" # Color 
          ggtheme = theme_gray()     # Change theme (theme_gray(), theme_bw(), theme_minimal(), theme_classic(), theme_void())
)




########################################
########################################
####   MORE ON US_ARRESTS DATASET   ####
########################################
########################################

# ############################################
# # AGNES using stats package (loaded by default, used with hclust and cutree method)
# ############################################
# # Normalize data
# # df <- USArrests
# df <- scale(USArrests)
# # View the firt 6 rows of the data
# head(df, n = 6)
# 
# # Dissimilarity matrix
# d <- dist(df, method = "euclidean")
# # Hierarchical clustering using complete method
# res.hc <- hclust(d, method = "complete" )
# # Plot the obtained dendrogram
# plot(res.hc, cex = 0.6, hang = -1)
# 
# # Cut tree into 4 groups
# grp <- cutree(res.hc, k = K)
# # Number of members in each cluster
# table(grp)
# # plot coloured rectangles on dendogram
# rect.hclust(res.hc, k = 4, border = 2:5)
# 
# 
# 
# # plot groups on 2D (from factoextra package)
# fviz_cluster(list(data = df, cluster = grp))





# ############################################################
# # AGNES with PCA for a 3D plot
# ############################################################
# 
# # Compute PCA with ncp = 3
# # We start by computing again the principal component analysis (PCA).
# # The argument ncp = 3 is used in the function PCA()
# # to keep only the first three principal components.
# # Next, the HCPC is applied on the result of the PCA.
# library(FactoMineR)
# res.pca <- PCA(USArrests, ncp = 3, graph = FALSE)
# 
# # The function HCPC() [in FactoMineR package] can be used to compute
# # hierarchical clustering on principal components.
# res.hcpc <- HCPC(res.pca, graph = FALSE)
# 
# 
# # from factoextra lib
# fviz_dend(res.hcpc,
#           cex = 0.7,                     # Label size
#           palette = "jco",               # Color palette see ?ggpubr::ggpar
#           rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
#           rect_border = "jco",           # Rectangle color
#           labels_track_height = 0.8      # Augment the room for labels
# )
# 
# # It’s possible to visualize individuals on the principal component
# # map and to color individuals according to the cluster they belong to.
# # The function fviz_cluster() [in factoextra] can be used to visualize individuals clusters.
# fviz_cluster(res.hcpc,
#              repel = TRUE,            # Avoid label overlapping
#              show.clust.cent = TRUE, # Show cluster centers
#              palette = "jco",         # Color palette see ?ggpubr::ggpar
#              ggtheme = theme_minimal(),
#              main = "Factor map"
# )
# 
# 
# # You can also draw a three dimensional plot combining the hierarchical clustering
# # and the factorial map using the R base function plot():
# # Principal components + tree:
# plot(res.hcpc, choice = "3D.map")