##########################################
##########################################
###### PREPARE DATA - CLEANING DATA ######
##########################################
##########################################


###################################################
###### Update Data Frame to Remove Outliers #######
###################################################

# load the libraries
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
# View(PimaIndiansDiabetes)
sum(is.na(PimaIndiansDiabetes))

# calculate stats for pregnant (number of times pregnant)
pregnant.mean <- mean(PimaIndiansDiabetes$pregnant)
pregnant.sd <- sd(PimaIndiansDiabetes$pregnant)
# max reasonable value is within 99.7% of the data (if Gaussian)
pregnant.max <- pregnant.mean + (3*pregnant.sd)
# mark outlier pregnant values as N/A
PimaIndiansDiabetes$pregnant[PimaIndiansDiabetes$pregnant>pregnant.max] <- NA
sum(is.na(PimaIndiansDiabetes))


#########################################
###### Mark Missing Values as N/A #######
#########################################

# load the libraries
library(mlbench)
# load the dataset
data(PimaIndiansDiabetes)
sum(is.na(PimaIndiansDiabetes))
# View(PimaIndiansDiabetes)

# mark a pressure of 0 as N/A, it is impossible
invalid <- 0
PimaIndiansDiabetes$pressure[PimaIndiansDiabetes$pressure==invalid] <- NA
# View(PimaIndiansDiabetes)
sum(is.na(PimaIndiansDiabetes))



####################################
###### Impute missing values #######
####################################

# load the libraries
library(Hmisc)
# impute missing pressure values using the mean
PimaIndiansDiabetes$pressure <- with(PimaIndiansDiabetes, impute(pressure, mean))
# View(PimaIndiansDiabetes)
sum(is.na(PimaIndiansDiabetes))


##################################
###### Remove rows with NA #######
##################################

# load library
library(mlbench)
# load dataset
data(BreastCancer)
# summarize dimensions of dataset
dim(BreastCancer)

cases_ok = sum(complete.cases(BreastCancer))
cases_ok
cases_ko = sum(!complete.cases(BreastCancer))
cases_ko

# Remove all incomplete rows
dataset <- BreastCancer[complete.cases(BreastCancer),]
# summarize dimensions of resulting dataset
dim(dataset)



#########################################
###### Remove Duplicate Instances #######
#########################################

# load the libraries
library(mlbench)
# load the dataset
data(iris)
dim(iris)
# remove duplicates
clean <- unique(iris)
dim(clean)