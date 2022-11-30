# https://www.ics.uci.edu/~smyth/courses/cs274/notes/EMnotes.pdf

# Clean enviroment
rm(list=ls())

# 1,3 non si vedono bene i 3 picchi

##############################
### Test 1 con 3 Clusters: ###
##############################
K = 3
data = iris[c(1,4)] # prendo solo due colonne per lavorare solo in 2D
##############################
### Test 2 con 4 Clusters: ###
##############################
# Rilancia decommentando questo blocco
# K=4
# data = USArrests[c(1,3)] # prendo solo due colonne per lavorare solo in 2D


##################
### Plot Data: ###
##################
par(mfrow=c(1,1)) # 1 disegno per plot
plot(data)
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
importPkgs(c("LaplacesDemon", "ggplot2", "MASS", "plotly", "threejs", "Rfast"))

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

drawOnlyIf2D = function(data, K, mu, Sigma, logL, clust_i, sleep=0.25, PlotZ=0){
  # http://www2.stat.duke.edu/~rcs46/lectures_2015/02-multivar2/02-multivar2.pdf
  if(dim(data)[2]==2 && !is.null(logL)){
    Sys.sleep(sleep)
    
    # mins = apply(data, 2, min)
    # maxs = apply(data, 2, max)
    N = dim(data)[1]
    D = dim(data)[2]
    
    # # Solo se D = 2
    samples = 5000
    bivn = array(rep(0, samples*D*K), dim=c(samples, D, K))
    for(k in 1:K){
      # print(paste("K=", k))
      # Generate sample from N(mu, Sigma)
      bivn[,,k] = mvrnorm(samples, mu = mu[,k], Sigma = Sigma[,,k])  # from Mass package
    }
    
    # Calculate kernel density estimate
    binvx = c()
    binvy = c()
    for(k in 1:K){
      binvx = c(binvx, bivn[,1,k])
      binvy = c(binvy, bivn[,2,k])
    }
    
    dens <- kde2d(binvx, binvy, n = 50)   # from MASS package
    
    if(PlotZ==0){
      par(mfrow=c(1, 3))
      # Contour plot overlayed on heat map image of results
      
      image(dens)       # from base graphics package
      contour(dens, add = TRUE)     # from base graphics package
      
      
      
      plot(data, main=length(logL), col = clust_i, pch=19)
      plot(logL)
      par(mfrow=c(1,1))
    } else if(PlotZ==1){
      # threejs Javascript plot package required
      # Unpack data from kde grid format
      x <- dens$x; y <- dens$y; z <- dens$z
      # Construct x,y,z coordinates
      xx <- rep(x,times=length(y))
      yy <- rep(y,each=length(x))
      zz <- z; dim(zz) <- NULL
      # Set up color range
      ra <- ceiling(16 * zz/max(zz))
      col <- rainbow(16, 2/3)
      # 3D interactive scatter plot
      scatterplot3js(x=xx, y=yy, z=zz, size=0.4, color = col[ra], bg="black")
    } else if(PlotZ==2){
      # plotly package required
      plot_ly(x = dens$x, y = dens$y, z = dens$z) %>% add_surface()
    }
  }
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
  
  N_z_i_k = array(rep(0, N*K), dim=c(N, K))
  N_z_i_k__pi_k = array(rep(0, N*K), dim=c(N, K))
  for(i in 1:N){
    for(k in 1:K){
      z_i = dataMatrix[i,]
      N_z_i_k[i,k] = dmvn(z_i, mu[,k], Sigma[,,k], log=FALSE)
      N_z_i_k__pi_k[i,k] = N_z_i_k[i,k]*mixin[k]
    }
  }
  
  x_ik = array(rep(0, N*K), dim=c(N, K))
  Sum_N_z_i_k__pi_k = array(rep(0, N*1), dim=c(N, 1))
  for(i in 1:N){
    for(k in 1:K){
      Sum_N_z_i_k__pi_k[i,1] = sum(N_z_i_k__pi_k[i,])
      x_ik[i,k] = N_z_i_k__pi_k[i,k]/Sum_N_z_i_k__pi_k[i,1]
    }
  }
  # Fuori da questo ultimo for rowSums(x_ik) = 1 per ogni i
  # rowSums(x_ik)
  
  return(list("x_ik" = x_ik,  "Sum_N_z_i_k__pi_k" = Sum_N_z_i_k__pi_k))
}


emAlg = function(data, K, logL_threshold=0.0000001, max_iterations=100000000, sleep=0.000001){
  
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
  iterations = 1
  logL = c()
  
  while(logL_changed_significantly == T && iterations < max_iterations){
    ########################
    ### (E) Expectation: ###
    ########################
    # Init N(z_i|...) 
    exp_result = expectation(data, K, mixin, mu, Sigma)
    x_ik = exp_result$x_ik
    
    #################
    ### (P) Plot: ###
    #################
    # Rfast required
    clust_i = rowMaxs(x_ik, value = FALSE) # assegnazione elemento i al cluster k (value=False cerca l'indice e non il valore)
    drawOnlyIf2D(data, K, mu, Sigma, logL, clust_i, sleep=sleep)
    
    #########################
    ### (M) Maximization: ###
    #########################
    sum_x_ik = colSums(x_ik)
    
    for(k in 1:K){
      # mixin update
      mixin[k] = sum_x_ik[k]/N
      
      # mu update
      mu[,k] = matrix(0, D, 1)
      for(i in 1:N){
        z_i = dataMatrix[i,]
        mu[,k] = mu[,k] + x_ik[i,k] * z_i
      }
      mu[,k] = mu[,k]/(N*mixin[k])
      
      # mu[,k] = colSums(x_ik[,k]*dataMatrix)/(N*mixin[k]) # --> 1 row update
      
      # Sigma update
      Sigma[,,k] = matrix(0, D, D)
      for(i in 1:N){
        z_i = dataMatrix[i,]
        difference = z_i - mu[,k]
        diffCol = array(difference, dim=c(D, 1))
        diffRow = array(difference, dim=c(1, D))
        Sigma[,,k] = Sigma[,,k] + x_ik[i,k] * diffCol%*%diffRow
      }
      Sigma[,,k] = Sigma[,,k]/(N*mixin[k])
    }
    
    ###########################
    ### (L) Log-Likelihood: ###
    ###########################
    exp_result = expectation(data, K, mixin, mu, Sigma)
    
    log_likelihood_old = ifelse(is.null(logL), -Inf, tail(logL, n=1))
    logL = c(logL, sum(log(exp_result$Sum_N_z_i_k__pi_k)))
    log_likelihood_new = tail(logL, n=1)
    
    logL_changed_significantly = (log_likelihood_new - log_likelihood_old) > logL_threshold
    
    print(paste("Interation #", iterations))
    print(paste("logL = ", log_likelihood_new))
    iterations = iterations + 1
  }
  
  # Compute final x_ik
  x_ik = expectation(data, K, mixin, mu, Sigma)$x_ik
  clust_i = rowMaxs(x_ik, value = FALSE)
  drawOnlyIf2D(data, K, mu, Sigma, logL, clust_i, sleep=sleep)
  
  return(
    list(
      "mixin" = mixin, 
      "mu" = mu, 
      "Sigma" = Sigma, 
      "logL" = logL, 
      "x_ik" = x_ik,
      "clust_i" = clust_i
      )
    )
}


emReturn = emAlg(data, K, sleep=0.000001)

# Set final mu, Sigma, logL and x_ik
mixin   = emReturn$mixin
mu      = emReturn$mu
Sigma   = emReturn$Sigma
logL    = emReturn$logL
x_ik    = emReturn$x_ik
clust_i = emReturn$clust_i

# Final plots
drawOnlyIf2D(data, K, mu, Sigma, logL, clust_i, PlotZ=0)
drawOnlyIf2D(data, K, mu, Sigma, logL, clust_i, PlotZ=1)
# le coordinate non corrisondono esattamente in questo plot...
drawOnlyIf2D(data, K, mu, Sigma, logL, clust_i, PlotZ=2)

