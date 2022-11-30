# Clean enviroment
rm(list=ls())
set.seed(1234)

library(scatterplot3d) # for scatterplot 3d
disp = mtcars$disp
mpg = mtcars$mpg

# Plot dei dati
plot(disp, mpg, col = "blue", pch = 20)

# Calcolo una regressione lineare per capire 
# quale dovrebbe essere il risultato finale analiticamente
model <- lm(mpg ~ disp, data = mtcars)
coef(model)

# Plot della retta ottenuta dalla regressione lineare
y_preds <- predict(model)
abline(model, lwd=3, lty=2)

# Quale è l'MSE ottimale ottenuto con la regressione lineare?
errors <- (mpg - y_preds) ^ 2
mean(errors) # oppure sum(errors) / length(mpg)

# GD function
gradientDesc <- function(x, y, learn_rate, conv_threshold, max_iter, iterations_for_plot=10000) {
  n = length(x)
  plot(x, y, col = "blue", pch = 20)
  
  # Disegno tratteggiata la linea che dovrebbe essere ottenuta
  # al termine dell'esecuzione della discesa del gradiente
  model <- lm(y ~ x)
  abline(model, lwd=3, lty=2)
  
  m <- runif(1, 0, 1)
  q <- runif(1, 0, 1)
  yhat <- m * x + q
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  
  # add points to dataframe
  df_points<-data.frame(m=m, q=q, MSE=MSE)
  
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- q - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    q <- c_new
    yhat <- m * x + q
    
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      abline(q, m) 
      converged = T
      return(df_points)
    }
    
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(q, m) 
      converged = T
      return(df_points)
    }
    
    # Add a new line plot each "iterations_for_plot" iterations
    if(iterations%%iterations_for_plot == 0){
      # matrix of plots
      par(mfrow=c(1, 3))

      # first plot
      plot(x, y, col = "blue", pch = 20)
      abline(model, lwd=3, lty=2)

      col = iterations/iterations_for_plot # from 0 to 10 is always a different color
      abline(q, m, col = col)
      
      
      # second plot
      plot(df_points$q, df_points$m)
      
      
      # third plot
      df_points[nrow(df_points) + 1,] = c(m, q, MSE_new)
      scatterplot3d(x=df_points$q, y=df_points$m, z=log(df_points$MSE), pch = 16, color="steelblue", type="h")
      # points3d(df_points$x, df_points$y, df_points$z)
      
      
      # add title for the page
      mtext(paste("Iteration=", iterations, 
                  ", q=", round(q, 2), 
                  ", m=", round(m, 2), 
                  ", MSE=", round(MSE_new, 2), sep = ""), outer=TRUE,  cex=1, line=-2.5)
      
      
      # Pauses the program for a small period of time
      Sys.sleep(0.05)
    }
  }
}


# Run the function 
gd_qm = gradientDesc(disp, mpg, 0.0000293, 0.001, 10000*150)
#gradientDesc(disp, mpg, 0.0000293, 0.001, 2500000)

q_opt = tail(gd_qm$q, n=1)
m_opt = tail(gd_qm$m, n=1)
paste("Optimal intercept:", q_opt, "Optimal slope:", m_opt)

# compare result with
coef(model)

# la differenza tra il risultato analitico e 
# quello ottenuto con gradient descent è marginale:
model$coefficients - c(q_opt, m_opt)