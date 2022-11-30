# Clean enviroment
rm(list=ls())

IsPure <- function(data) {
  length(unique(data[,ncol(data)])) == 1
}


Entropy <- function( vls ) {
  res <- vls/sum(vls) * log2(vls/sum(vls))
  res[vls == 0] <- 0
  -sum(res)
}

Entropy(c(10, 0))
Entropy(c(0, 10))
Entropy(c(5, 5))
Entropy(c(2, 3))


entropy <- function(edible) Entropy(c(edible, 100 - edible))
entropy <- Vectorize(entropy)
curve( entropy, from = 0, to = 100, xname = 'edible')


# x = c(0, 100)
# a = Entropy(x)
# a
# 
# res <- x/sum(x) * log2(x/sum(x))
# res


InformationGain <- function( tble ) {
  tble <- as.data.frame.matrix(tble)
  entropyBefore <- Entropy(colSums(tble))
  s <- rowSums(tble)
  entropyAfter <- sum (s / sum(s) * apply(tble, MARGIN = 1, FUN = Entropy ))
  informationGain <- entropyBefore - entropyAfter
  return (informationGain)
}





# DATA
weather <- expand.grid(Outlook = c("Sunny","Overcast","Rain"),
                       Temperature = c("Hot","Mild", "Cool"),
                       Humidity=c("High", "Normal"), Wind=c("Weak","Strong"))
dim(weather)


# slides images example:
response <- c(1, 19, 2, 6, 18, 36, 35, 4, 16, 15, 31, 23, 11, 24)
play <- as.factor(c("No", "No", "Yes", "Yes", "Yes", "No",
                    "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No"))
tennis <- data.frame(weather [response, ], play)
tennis



# TREE
TrainID3 <- function(node, data) {
  
  node$obsCount <- nrow(data)
  
  #if the data-set is pure (e.g. all yes/no or toxic in mushroom example), then
  if (IsPure(data)) {
    #construct a leaf having the name of the pure feature (e.g. 'toxic')
    child <- node$AddChild(unique(data[,ncol(data)]))
    node$feature <- tail(names(data), 1)
    child$obsCount <- nrow(data)
    child$feature <- ''
  } else {
    #chose the feature with the highest information gain (e.g. 'color')
    ig <- sapply(colnames(data)[-ncol(data)], 
                 function(x) InformationGain(
                   table(data[,x], data[,ncol(data)])
                 )
    )
    feature <- names(ig)[ig == max(ig)][1]
    
    node$feature <- feature
    
    #take the subset of the data-set having that feature value
    childObs <- split(data[,!(names(data) %in% feature)], data[,feature], drop = TRUE)
    
    for(i in 1:length(childObs)) {
      #construct a child having the name of that feature value (e.g. 'red')
      child <- node$AddChild(names(childObs)[i])
      
      #call the algorithm recursively on the child and the subset      
      TrainID3(child, childObs[[i]])
    }
    
  }
  
}


library(data.tree)
tree <- Node$new("playOrNot")
TrainID3(tree, tennis)
print(tree, "feature", "obsCount")
