library(meanShiftR)
library(LPCM)

# set a seed to make this reproducible 
set.seed(100)

# set the number of iterations to test 
# (we will terminate well before this)
iter <- 1000

# set the number of points to simulate
n <- 500

# set the bandwidth
# prova a sostituire h con c(0.25,0.25)
h <- c(0.5,0.5)

# create example data
x1 <- matrix(rnorm( n ), ncol=2)
x2 <- matrix(rnorm( n ), ncol=2) + 2
x  <- rbind( x1, x2 )

#plot initial points
plot(x, col=rep(c('red','green'), each=n/2), cex=2, xlab='x', ylab='y', pch=20)


########### meanShiftR ###################
result <- meanShift(
  x,
  x,
  bandwidth=h,
  iterations = iter
)

# assignment
meanShiftR_assignment <- result$assignment

# value
meanShiftR_value <- result$value


########### LPCM ###################
result <- ms(
  x,
  h=h,
  scaled=FALSE,
  iter=iter)

# assignment
LPCM_assignment <- result$cluster.label

# value
LPCM_value <- result$cluster.center[LPCM_assignment,]


# compare the two results
meanShiftR_assignment - LPCM_assignment
sum(meanShiftR_assignment - LPCM_assignment) == 0



######################
### Add test data: ###
######################

# set the number of iterations to test 
# (we will terminate well before this)
iter <- 10

# set the number of points to simulate
m <- 20

# create some test data for diagnostic plots
y1 <- matrix( rnorm( m ), ncol=2)
y2 <- matrix( rnorm( m ), ncol=2) + 2
y <- rbind( y1, y2 )

plot(x, col=rep(c('salmon','greenyellow'),each=n/2), cex=1.5, xlab='x',ylab='y',pch=20)

points(y, col=rep(c('red','green'),each=m/2), cex=2, pch=19)
points(y, cex=2)

########### meanShiftR ###################


#initial run
result <- meanShift(
  y,
  x,
  # algorithm="KDTREE",
  bandwidth = h,
  iterations = iter,
  # parameters=c(10,100)
)

y0 <- rbind(y,result$value)

for( i in 2:iter) {
  result <- meanShift(
    result$value,
    x,
    bandwidth=h,
    iterations = 1
  )
  
  # concate on the result
  y0 <- rbind(y0, result$value)
}

# plot the paths

for( i in 1:m ) {
  pointIndex <- seq(from=0,to=(m*(iter-1)),by=m)+i
  points(y0[pointIndex,] , type='l', lwd=2)
}

