# Clean enviroment
rm(list=ls())

##############################################################
# LOADS AN IMAGE
##############################################################
library(png)
mandrillFile  = "data/images/mandrill.png"
mandrill      = readPNG(mandrillFile)


##############################################################
# CREATE A DATAFRAME WITH RGB OF THE IMAGE
##############################################################
# Just trying to see if hierarchical is feasible
img = mandrill
df = data.frame(
  red = matrix(img[,,1], ncol=1),
  green = matrix(img[,,2], ncol=1),
  blue = matrix(img[,,3], ncol=1)
)


##############################################################
# LAUNCH AGNES ON THAT DATAFRAME
##############################################################
library(cluster)
res.agnes <- agnes(x = df, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "complete" # Linkage method
)
# --> You'll get the error: "Error: vector memory exhausted (limit reached?)"


##############################################################
# WHY IT'S GIVING "Error: vector memory exhausted (limit reached?)"?
##############################################################
# The lenght of the dataframe is 90.000
dim(df)

# ~90.000*90.000 is too much for R ~= 8.1e+09
library(pryr)
myVar = 128
class(myVar)
object.size(myVar) # size of a single numeric variable

x = 1:(90000*90000) # let's create a simple vector with 90000*90000 elements (as a matrix of 90000 rows and cols)
# object_size(x[1])*(90000*90000)
object_size(x) # if the memory it will take after a first edit of the x vector
# x[3] = myVar # --> if you execute that it'll really drain all that amount of memory

class(myVar)
object_size(myVar)

object_size(df$red[1])
object_size(df$red[2])
