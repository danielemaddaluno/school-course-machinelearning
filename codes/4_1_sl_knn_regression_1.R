# Su Windows per scrivere il carattere tilde (~):
# ALT + 0126 (tenete premuto ALT e digitate 0126 con il tastierino numerico)
#
# Su Mac per scrivere il carattere tilde (~):
# ALT + 5

# Clean enviroment
rm(list=ls())
set.seed(42)

library(MASS)
?Boston
dim(Boston)
head(Boston)


# Housing Values in Suburbs of Boston (east coast U.S.A.) in 1978
# Boston is the capital and most populous city of Massachusetts.
# This dataset contains information collected by the U.S Census Service 
# concerning housing in the area of Boston Mass. It was obtained from the 
# StatLib archive (http://lib.stat.cmu.edu/datasets/boston), 
# and has been used extensively throughout the literature to benchmark algorithms.
# The dataset is small in size with only 506 cases.
#
# LSTAT - % lower status of the population                --> percentuale a basso reddito della popolazione
# MEDV - Median value of owner-occupied homes in $1000's  --> valore medio delle abitazioni in termini di migliaia di $

# Ci focalizzeremo solo su queste due colonne del dataset (il dataset ne ha molte altre), 
# la prima (lstat) come feature, la seconda (medv) come label
head(Boston[,c("lstat", "medv")])

# Plot lstat on x and medv as y
#
# plot(y ~ x, data = trn_boston)
# plot(medv ~ lstat, data = trn_boston)
#
# ==
# plot(x, y)
# plot(trn_boston$lstat, trn_boston$medv)
par(mfrow = c(1, 1))
plot(medv ~ lstat, data = Boston, col = "dodgerblue", main = "Median value of homes in $1000")
# plot(Boston$lstat, Boston$medv)

# Campiono randomicamente 250 valori tra 1:506: usero questi indici per dividere
# il mio dataset iniziale tra training e testing set
boston_idx = sample(1:nrow(Boston), size = 250)
length(boston_idx)

# training set come dataframe
trn_boston = Boston[boston_idx, ]
# dim(trn_boston)

# testing set come dataframe
tst_boston  = Boston[-boston_idx, ]
# dim(tst_boston)

# Training set features (X) and label (Y)
X_trn_boston = trn_boston["lstat"]
y_trn_boston = trn_boston["medv"]

# Testing set features (X) and label (Y)
X_tst_boston = tst_boston["lstat"]
y_tst_boston = tst_boston["medv"]


######################################################################
######################################################################
#####  VALUTIAMO LE PERFORMANCE DI VARI MODELLI KNN SUL TEST SET #####
######################################################################
######################################################################

# To perform KNN for regression, we will need `knn.reg()` from the `FNN` package.
library(FNN)
?knn.reg

pred_001 = knn.reg(train = X_trn_boston, test = X_tst_boston, y = y_trn_boston, k = 1)
pred_005 = knn.reg(train = X_trn_boston, test = X_tst_boston, y = y_trn_boston, k = 5)
pred_010 = knn.reg(train = X_trn_boston, test = X_tst_boston, y = y_trn_boston, k = 10)
pred_050 = knn.reg(train = X_trn_boston, test = X_tst_boston, y = y_trn_boston, k = 50)
pred_100 = knn.reg(train = X_trn_boston, test = X_tst_boston, y = y_trn_boston, k = 100)
pred_250 = knn.reg(train = X_trn_boston, test = X_tst_boston, y = y_trn_boston, k = 250)


errMSE = function(y, y_hat){
  return ( sum((y-y_hat)^2)/length(y) )
}

k_vals = c()
error_MSE = c()
for(pred_item in list(pred_001, pred_005, pred_010, pred_050, pred_100, pred_250)){
  k = pred_item$k
  k_error_MSE = errMSE(y_tst_boston, pred_item$pred)
  message(sprintf("K=%d \t--> MSE = %f", k, k_error_MSE))
  
  k_vals = c(k_vals, k)
  error_MSE = c(error_MSE, k_error_MSE)
}

par(mfrow = c(1, 1))
plot(k_vals, error_MSE, col = "dodgerblue", type="b", main = "k vs MSE on testing set")


####################################################################################
####################################################################################
######  OSSERVIAMO LA CURVA OTTENUTA PER OGNI MODELLO SU UNA GRIGLIA DI PUNTI ######
####################################################################################
####################################################################################

# Applichiamo KNN con vari K e valutiamone l'errore su un test set:
# Proviamo a dare una misura dell'errore sul test set

# We create an additional "test" set `test_grid`, that is a grid of `lstat` 
# values at which we will predict `medv` in order to create graphics.
X_trn_boston_min = min(X_trn_boston)
X_trn_boston_max = max(X_trn_boston)
message(sprintf("min=%f \t max=%f", X_trn_boston_min, X_trn_boston_max))
test_grid = data.frame(lstat = seq(X_trn_boston_min, X_trn_boston_max, by = 0.01))
dim(test_grid)

pred_001 = knn.reg(train = X_trn_boston, test = test_grid, y = y_trn_boston, k = 1)
pred_005 = knn.reg(train = X_trn_boston, test = test_grid, y = y_trn_boston, k = 5)
pred_010 = knn.reg(train = X_trn_boston, test = test_grid, y = y_trn_boston, k = 10)
pred_050 = knn.reg(train = X_trn_boston, test = test_grid, y = y_trn_boston, k = 50)
pred_100 = knn.reg(train = X_trn_boston, test = test_grid, y = y_trn_boston, k = 100)
pred_250 = knn.reg(train = X_trn_boston, test = test_grid, y = y_trn_boston, k = 250)


# We make predictions for a large number of possible values of `lstat`, 
# for different values of `k`. Note that `250` is the total number of observations 
# in this training dataset.
par(mfrow = c(3, 2))

plot(medv ~ lstat, data = trn_boston, col = "dodgerblue", main = "k = 1")
lines(test_grid$lstat, pred_001$pred, col = "darkorange", lwd = 0.25)

plot(medv ~ lstat, data = trn_boston, col = "dodgerblue", main = "k = 5")
lines(test_grid$lstat, pred_005$pred, col = "darkorange", lwd = 0.75)

plot(medv ~ lstat, data = trn_boston, col = "dodgerblue", main = "k = 10")
lines(test_grid$lstat, pred_010$pred, col = "darkorange", lwd = 1)

plot(medv ~ lstat, data = trn_boston, col = "dodgerblue", main = "k = 50")
lines(test_grid$lstat, pred_050$pred, col = "darkorange", lwd = 1.5)

plot(medv ~ lstat, data = trn_boston, col = "dodgerblue", main = "k = 100")
lines(test_grid$lstat, pred_100$pred, col = "darkorange", lwd = 2)

plot(medv ~ lstat, data = trn_boston, col = "dodgerblue", main = "k = 250")
lines(test_grid$lstat, pred_250$pred, col = "darkorange", lwd = 2)

# We see that `k = 1` is clearly overfitting, as `k = 1` is a very complex, 
# highly variable model. Conversely, `k = 250` is clearly underfitting the data, 
# as `k = 250` is a very simple, low variance model. In fact, here it is predicting 
# a simple average of all the data at each point.



##########################################################################################
##########################################################################################
######  OSSERVIAMO QUANTO UN MODELLO KNN SI ADATTA BENE ALLE VARIE FORME FUNZIONALI ######
##########################################################################################
##########################################################################################


# Facciamo qualche osservazione nel caso in cui volessi cercare di approssimare una
# distribuzione originata dalle forme funzionali: x, x^2 o sin(x)
line_reg_fun = function(x) {
  x
}

quad_reg_fun = function(x) {
  x ^ 2
}

sine_reg_fun = function(x) {
  sin(x)
}

get_sim_data = function(f, sample_size = 100, sd = 1) {
  x = runif(n = sample_size, min = -5, max = 5)
  y = rnorm(n = sample_size, mean = f(x), sd = sd)
  data.frame(x, y)
}

set.seed(42)
line_data = get_sim_data(f = line_reg_fun)
quad_data = get_sim_data(f = quad_reg_fun, sd = 2)
sine_data = get_sim_data(f = sine_reg_fun, sd = 0.5)

x_grid = data.frame(x = seq(-5, 5, by = 0.01))

par(mfrow = c(1, 3))
plot(y ~ x, data = line_data, pch = 1, col = "darkgrey", main = expression("y=x"))
grid()
knn_pred = knn.reg(train = line_data$x, test = x_grid, y = line_data$y, k = 10)$pred
lines(x_grid$x, line_reg_fun(x_grid$x), lwd = 2)
lines(x_grid$x, knn_pred, col = "darkorange", lwd = 2)

plot(y ~ x, data = quad_data, pch = 1, col = "darkgrey", main = expression("y=x"^2))
grid()
knn_pred = knn.reg(train = quad_data$x, test = x_grid, y = quad_data$y, k = 10)$pred
lines(x_grid$x, quad_reg_fun(x_grid$x), lwd = 2)
lines(x_grid$x, knn_pred, col = "darkorange", lwd = 2)

plot(y ~ x, data = sine_data, pch = 1, col = "darkgrey", main = expression("y=sin(x)"))
grid()
knn_pred = knn.reg(train = sine_data$x, test = x_grid, y = sine_data$y, k = 10)$pred
lines(x_grid$x, sine_reg_fun(x_grid$x), lwd = 2)
lines(x_grid$x, knn_pred, col = "darkorange", lwd = 2)

# k was reasonably well chosen
# this is a reasonable amount of data
# this is a rather low dimensional problem

