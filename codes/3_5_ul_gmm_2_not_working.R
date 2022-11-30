# https://www.ics.uci.edu/~smyth/courses/cs274/notes/EMnotes.pdf

# Clean enviroment
rm(list=ls())

data = iris[3:4]
#data = iris[1:4]
#data = head(iris[1:4], 6)
data = na.omit(data)
#data = scale(data)
head(data)

# Function to load multiple packages
importPkgs <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
      install.packages(package)
      library(package, character.only=T)
    }
  }
}

# import libraries
importPkgs(c("LaplacesDemon", "ggplot2", "MASS"))

# https://gist.github.com/casallas/1b0e288a79acf290fc0f
rep.row <-function(x,n){
  matrix(rep(x, n), nrow = n, byrow = T)
}

# https://stackoverflow.com/a/10961206/3138238
rep.col <-function(x,n){
  array(rep(x, n), dim=c(length(x), n))
}

seq.exclude <- function(from = 0, to = 1, length=2){
  base = seq(from, to, length=length+2)
  n = length(base)
  base[1:length+1]
}

init.mu <- function(data, K){
  N = dim(data)[1]
  D = dim(data)[2]
  mins = apply(data, 2, min)
  maxs = apply(data, 2, max)
  
  mu = array(rep(0, D*K), dim=c(D, K))
  for(i in 1:D){
    Dseq = seq.exclude(from=mins[i], to=maxs[i], K)
    mu[i,] = Dseq
  }
  
  return(mu)
}

# see "dmvn gives the density and rmvn generates random deviates."
# https://www.rdocumentation.org/packages/LaplacesDemon/versions/16.1.4/topics/dist.Multivariate.Normal
# dmvn(x, mu, Sigma, log=FALSE) 
# x:      This is data or parameters in the form of a vector of length K×K or a matrix with K columns.
# n:      This is the number of random draws.
# mu:     This is mean vector μ with length K or matrix with K columns.
# Sigma:  This is the K×K covariance matrix Σ.
# log:    Logical. If log=TRUE, then the logarithm of the density is returned.
expectation = function(data, K, mixin, mu, Sigma){
  N = dim(data)[1]
  D = dim(data)[2]
  dataMatrix = as.matrix(data)
 
  densI = array(rep(0, N*K), dim=c(N, K))
  pi_N_1_2__K = array(rep(0, N*K), dim=c(N, K))
  for(k in 1:K){
    # N(z_i|...): calcolo la densità di prob per ogni esempio z_i per ogni k, 
    # il risultato è un vettorone con un numero per ogni esempio (componendo infine una matrice)
    densI[,k] = dmvn(dataMatrix, mu[,k], Sigma[,,k], log=FALSE)
    pi_N_1_2__K[,k] = densI[,k]*mixin[k]
  }
  
  Sum_pi_N_1_2__K = rowSums(pi_N_1_2__K)
  x_ik = pi_N_1_2__K/rep.col(Sum_pi_N_1_2__K, K)  # x_ik è tale che rowSums(x_ik) vale uno per ogni i
  
  return(list("x_ik" = x_ik,  "Sum_pi_N_1_2__K" = Sum_pi_N_1_2__K))
}


emAlg = function(data, K, logL_threshold=0.0000001, max_iterations=100000000){
  
  ###########################
  ### (I) Initialization: ###
  ###########################
  N = dim(data)[1]                                # N:      num of features
  D = dim(data)[2]                                # D:      num of data
  
  mixin = rep(1/K, K) #1:K #rep(0, K)             # mixin:  scalars
  mu = init.mu(data, K)                           # mu:     K vectors column
  Sigma = array(rep(diag(D), K), dim=c(D, D, K))  # Sigma:  K matrices
  
  # converto data in una matrice: righe-->esempi, colonne-->features
  dataMatrix = as.matrix(data)
  
  logL_changed_significantly = T
  iterations = 0
  log_likelihood_new = -Inf
  
  while(logL_changed_significantly == T && iterations < max_iterations){
    ########################
    ### (E) Expectation: ###
    ########################
    # Init N(z_i|...) 
    exp_result = expectation(data, K, mixin, mu, Sigma)
    x_ik = exp_result$x_ik
    
    #########################
    ### (M) Maximization: ###
    #########################
    sum_x_ik = colSums(x_ik)
    
    for(k in 1:K){
      # mixin update
      mixin[k] = sum_x_ik[k]/N
      
      # mu update
      mu[,k] = colSums(x_ik[,k]*dataMatrix)/(N*mixin[k])
      
      # Sigma update
      Sigma[,,k] = matrix(0, D, D)
      for(i in 1:N){
        difference = dataMatrix[i,] - mu[,k]
        diffCol = array(difference, dim=c(D, 1))
        diffRow = array(difference, dim=c(1, D))
        Sigma[,,k] = Sigma[,,k] + x_ik[i,k]* diffCol%*%diffRow
      }
    }
    
    ###########################
    ### (L) Log-Likelihood: ###
    ###########################
    log_likelihood_old = log_likelihood_new
    exp_result = expectation(data, K, mixin, mu, Sigma)
    log_likelihood_new = sum(log(exp_result$Sum_pi_N_1_2__K))
    
    logL_changed_significantly = (log_likelihood_new - log_likelihood_old) > logL_threshold
    iterations = iterations + 1
    print(paste("Interation #", iterations))
    print(paste("logL = ", log_likelihood_new))
  }

  return(list("mu" = mu, "Sigma" = Sigma))
}

K = 3
emReturn = emAlg(data, K)
mu = emReturn$mu
Sigma = emReturn$Sigma




# http://www2.stat.duke.edu/~rcs46/lectures_2015/02-multivar2/02-multivar2.pdf
if(dim(data)[2]==2){
  # mins = apply(data, 2, min)
  # maxs = apply(data, 2, max)
  N = dim(data)[1]
  D = dim(data)[2]
  
  # Solo se D = 2
  samples = 5000
  bivn = array(rep(0, samples*D*K), dim=c(samples, D, K))
  for(k in 1:K){
    print(paste("K=", k))
    # Generate sample from N(mu, Sigma)
    bivn[,,k] = mvrnorm(samples, mu = mu[,k], Sigma = Sigma[,,k] )  # from Mass package
  }
  
  # Calculate kernel density estimate
  bivn.kde <- kde2d(c(bivn[,1,1], bivn[,1,2], bivn[,1,3]), 
                    c(bivn[,2,1], bivn[,2,2], bivn[,2,3]), n = 50)   # from MASS package
  # Contour plot overlayed on heat map image of results
  image(bivn.kde)       # from base graphics package
  contour(bivn.kde, add = TRUE)     # from base graphics package
}