##########################
##########################
###### LOADING DATA ######
##########################
##########################


##########################################
###### Loading R Built-in Data Sets ######
##########################################

# Datasets from the dataset library

# list the contents of the library
library(help = "datasets")

# list all available datasets in all loaded libraries
data()

# Iris flowers datasets
# see https://en.wikipedia.org/wiki/Iris_flower_data_set
data(iris)
?iris
View(iris)
dim(iris)
levels(iris$Species)
head(iris)


# Longley's Economic Regression Data
# data(longley)
# dim(longley)
# head(longley)



######################################################
###### Loading R Data Sets from mlbench library ######
######################################################

# Datasets from the mlbench library

# load the library
library(mlbench)

# list the contents of the library
library(help = "mlbench")

# Boston Housing dataset
data(BostonHousing)
?BostonHousing
dim(BostonHousing)
head(BostonHousing)


# And many many other datasets

# # Wisconsin Breast Cancer dataset
# data(BreastCancer)
# dim(BreastCancer)
# levels(BreastCancer$Class)
# head(BreastCancer)
# 
# # Pima Indians Diabetes dataset
# data(PimaIndiansDiabetes)
# dim(PimaIndiansDiabetes)
# levels(PimaIndiansDiabetes$diabetes)
# head(PimaIndiansDiabetes)
# 
# # Soybean dataset
# data(Soybean)
# dim(Soybean)
# levels(Soybean$Class)
# head(Soybean)



########################################################################
###### Loading R Data Sets from CSV (comma-separated values) file ######
########################################################################

# define the filename
filename <- "data/iris.csv"
# load the CSV file from the local directory
dataset <- read.csv(filename, header=FALSE)
# preview the first 5 rows
head(dataset)



########################################################################
###### Loading R Data Sets from CSV (comma-separated values) URL #######
########################################################################

# load the library
library(RCurl)
# specify the URL for the Iris data CSV
urlfile <-'https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data'
# download the file
downloaded <- getURL(urlfile, ssl.verifypeer=FALSE)
# treat the text data as a steam so we can read from it
connection <- textConnection(downloaded)
# parse the downloaded data as CSV
dataset <- read.csv(connection, header=FALSE)
# preview the first 5 rows
head(dataset)
