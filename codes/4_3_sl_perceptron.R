# Clean enviroment
rm(list=ls())

library(rgl)

# Change the seed to see different results
set.seed(1)

# Random.Unit returns n dim-dimensional points in the unit interval labeled
# -1 or 1. The label depends on whether or not the sum of the vector's 
# components exceeds the given threshold. That is, -1 if sum(vector) < threshold
# and 1 otherwise.
Random.Unit <-function(n, dim, threshold) {
  points <- runif(n * dim)
  points <- matrix(points, ncol = dim)
  # setto la label distinguendo la posizione di 1*x+1*y+1*z < threshold  ---> -1.5 + 1*x_1+1*x_2+1*x_3
  label <- ifelse(apply(points, 1, sum) < threshold, -1, 1)
  return(cbind(label, x0 = rep(1, n), points))
}

# Classify is our simple classification rule for the perceptron.We simply 
# return the sign of the dot-product of our observations and weights
Classify <- function(x, weights) {
  return(sign(x %*% weights))
}

# Perceptron is a simple implementation of the perceptron learning algorithm.
# It accepts data of the form data[1] = label, data[2] = x_0 = 1, data[3] = x_1,
# etc. w0 is initilized to -threshold and the weights returned are such that
# sign(w_0 * x_0 + w_1 * x_1 + ... + w_n * x_n) == label
Perceptron <- function(data, threshold, sleep=0.2, printW=T) {
  
  w <- c(-threshold, runif(ncol(data) - 2)) # il -2 è dovuto alla colonna label e x0
  n <- nrow(data)
  label <- data[ , 1]
  obs <- data[ , 2:ncol(data)] # prendo solo le colonne di x0 (+1), x1, x2, x3
  misclassfied <- TRUE
  while (misclassfied) {
    misclassfied <- FALSE
    for (i in 1:n) {
      if (label[i] * Classify(obs[i , ], w) <= 0) { # classificazione sbagliata 
        w <- w + label[i] * obs[i , ]
        misclassfied <- TRUE
        # break
      }
    }
    
    #added by me
    Sys.sleep(sleep)
    Plot3D(pts, w[4], w[3], w[2], w[1])
    #added by me
    if(printW){
      print(w)
    }
  }
  return(w)
}

# Plot3D is essentially a wrapper for the rgl package's plot3d function.
# It plots the result of a call to Random.Unit with dim = 3
# as well as the plane parameterized by ax + by + cz + d = 0
Plot3D <- function(points, a, b, c, d) {
  plot3d(points[, 3:5], xlab = "X", ylab = "Y", zlab = "Z",
         pch = ifelse(points[, 1] == 1, 2, 8),
         col = ifelse(points[, 1] == 1, "blue", "red"))
  planes3d(a, b, c, d)
}


THRESHOLD <- 1.5
pts <- Random.Unit(1000, 3, THRESHOLD)
head(pts)

Plot3D(pts)


w <- Perceptron(pts, THRESHOLD)
w # notare che w0 è -1.5 volte il resto dei w1, w2, w3 (per costruzione dei punti random iniziali)

Plot3D(pts, w[4], w[3], w[2], w[1])

# Per costruzione i punti sono stati separati usando sign(x+y+z-threshold) con la soglia =  1.5
# Plot3D(pts, 1, 1, 1, -THRESHOLD)
