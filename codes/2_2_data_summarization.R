###########################################
###########################################
###### ANALYZE DATA - SUMMARIZE DATA ######
###########################################
###########################################


#################################
###### Class Distribution #######
#################################

# load the libraries
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# distribution of class variable
y <- PimaIndiansDiabetes$diabetes
# column bind between freq and percentage
cbind(freq=table(y), percentage=prop.table(table(y))*100)



#########################
###### Data Types #######
#########################

# list types for each attribute
sapply(iris, class)


#########################
###### Dimensions #######
#########################

# display the dimensions of the dataset
dim(iris)


#####################
###### Peek #########
#####################

# Peek at raw data
# display first 20 rows of data
head(iris, n=20)


######################
###### Summary #######
######################

# Summarize each attribute of a dataset using min, max, mean, 25%, 50% and 75%.
# summarize the dataset
summary(iris)
