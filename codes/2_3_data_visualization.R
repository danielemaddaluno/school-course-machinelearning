###########################################
###########################################
###### ANALYZE DATA - VISUALIZE DATA ######
###########################################
###########################################


########################
########################
###### UNIVARIATE ######
########################
########################

#######################################
###### Univariate - Plot Factor #######
#######################################

# load the library
library(mlbench)
# load the dataset
data(BreastCancer)
# create a bar plot of each categorical attribute
par(mfrow=c(2,4))
for(i in 2:9) {
  counts <- table(BreastCancer[,i])
  name <- names(BreastCancer)[i]
  barplot(counts, main=name)
}



#################################################
###### Univariate - Box And Whisker Plots #######
#################################################

# load dataset
data(iris)
# Create separate boxplots for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(iris[,i], main=names(iris)[i])
}



#########################################
###### Univariate - Density Plots #######
#########################################

# load libraries
library(lattice)
# load dataset
data(iris)
# create a panel of simpler density plots by attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  plot(density(iris[,i]), main=names(iris)[i])
}



######################################
###### Univariate - Histograms #######
######################################

# load the data
data(iris)
# create histograms for each attribute
par(mfrow=c(1,4))
for(i in 1:4) {
  hist(iris[,i], main=names(iris)[i])
}


###########################################
###### Univariate - Reset Par Mfrow #######
##########################################

# to reset par mfrow:
dev.off()
# or
# par(mfrow=c(1,1))



##########################
##########################
###### MULTIVARIATE ######
##########################
##########################

################################################
###### Multivariate - Scatterplot Matrix #######
################################################
# Multivariate 

# load the data
data(iris)
# pair-wise scatterplots of all 4 attributes
pairs(iris)


############################################################
###### Multivariate - Create a box and whisker plots #######
######     for each variable organized by class      #######
############################################################

# load the caret library
library(caret)
# load the iris dataset
data(iris)
# box and whisker plots for each attribute by class value
x <- iris[,1:4]
y <- iris[,5]
featurePlot(x=x, y=y, plot="box")



##############################################
###### Multivariate - Correlation Plot #######
##############################################

# load library
library(corrplot)
# load the data
data(iris)
# calculate correlations
correlations <- cor(iris[,1:4])
# create correlation plot
corrplot(correlations, method="circle")



#######################################################
###### Multivariate - Create a density plot for #######
######     each variable-class combination.     #######
#######################################################

# load the library
library(caret)
# load the data
data(iris)
# density plots for each attribute by class value
x <- iris[,1:4]
y <- iris[,5]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)




#########################################################
###### Multivariate - Scatterplot Matrix By Class #######
#########################################################

# load the data
data(iris)
# pair-wise scatterplots colored by class
pairs(Species~., data=iris, col=iris$Species)
plot(iris[,1:4], col=iris[,5])