# Clean enviroment
rm(list=ls())

# Set the seed
set.seed(12345678)

# In this document, we are going to explore the K-means Clustering algorithm in R. 
# We will be working with the iris dataset.
# K-means Clustering is used with unlabeled data, but in this case, 
# we have a labeled dataset so we have to use the iris data without the Species column. 
# In this way, algorithm will cluster the data and we will be able to compare the 
# predicted results with the original results, getting the accuracy of the model.
df <- iris
head(iris)

# Let’s make a scatterplot.
plot(df$Petal.Length, df$Petal.Width, col=df$Species, pch=19)
legend("topleft", fill = unique(df$Species), legend = c(levels(df$Species)), cex = 1.2)

# library(ggplot2)
# ggplot(df, aes(Petal.Length, Petal.Width)) + geom_point(aes(col=Species), size=4)

# As we can see, setosa is going to be clustered easier. 
# Meanwhile, there is noise between versicolor and virginica even when they 
# look like perfectly clustered.
# Let’s run the model. kmeans is installed in the base package from R, 
# so we don’t have to install any package.
# In the kmeans function, it is necessary to set center, which is the number 
# of groups we want to cluster to. In this case, we know this value will be 3. 
# Let’s set that, but let’s see how we would build the model if we didn’t know it.
K <- 3
irisCluster <- kmeans(df[,1:4], K)
# irisCluster

plot(df$Petal.Length, df$Petal.Width, col=irisCluster$cluster, pch=19)
# why a red point? --> rember that this is just a projection on 2 variables (Petal.Length, Petal.Width)
legend("topleft", fill = unique(irisCluster$cluster), legend = c(unique(irisCluster$cluster)), cex = 1.2)


# We can compare the predicted clusters with the original data.
table(irisCluster$cluster, iris$Species)

# # We can plot out these clusters.
# library(cluster)
# clusplot(iris, irisCluster$cluster, color=T, shade=T, labels=0, lines=0)


# # We can see the setosa cluster perfectly explained, meanwhile virginica and versicolor have a little noise between their clusters.
# # As I said before, we will not always have the labeled data. If we would want to know the exactly number of centers, we should have built the elbow method.
# 
# tot.withinss <- vector(mode="character", length=10)
# for (i in 1:10){
#   irisCluster <- kmeans(df[,1:4], center=i, nstart=20)
#   tot.withinss[i] <- irisCluster$tot.withinss
# }
# 
# # Let’s visualize it.
# plot(1:10, tot.withinss, type="b", pch=19)

