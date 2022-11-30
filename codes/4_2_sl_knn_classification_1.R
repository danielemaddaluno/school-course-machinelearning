# Clean enviroment
rm(list=ls())

##############################################################################
##############################################################################
######## IRIS DATASET FILTER ON Sepal.Length, Sepal.Width and class   ########
##############################################################################
##############################################################################
# View(iris)
train = iris[,c("Sepal.Length", "Sepal.Width")]
cl = iris$Species


# # Matrice dei grafici scatter
# # Legenda: 1 = Setosa = Nero, 2 = Versicolor = Rosso, 3 = Virginica= Verde
# levels(cl)
# unclass(cl)
# 
# pairs(iris, col = cl, pch = 20, oma=c(3,3,3,16))
# par(xpd = TRUE)
# legend("bottomright", fill = unique(iris$Species), legend = c( levels(iris$Species)), cex = 0.70)

plot(train, col=cl, pch=19)
legend("topright", fill = unique(cl), legend = c(levels(cl)), cex = 0.9)



##############################################################################
##############################################################################
####### Creiamo una griglia e applichiamo la classificazione su questa #######
##############################################################################
##############################################################################

# Creazione griglia di test intorno al minimo e massimo delle colonne prese in considerazione
require(MASS)
test <- expand.grid(x=seq(min(train[,1]-1), max(train[,1]+1), by=0.1),
                    y=seq(min(train[,2]-1), max(train[,2]+1), by=0.1))
dim(test)
# View(test)


# applicazione del knn per k=3 e classificazione, con calcolo delle probabilità di assegnamento
require(class)

classif <- knn(train, test, cl, k = 3, prob=TRUE)
# View(classif)
length(classif)

prob <- attr(classif, "prob")
# View(prob)
length(prob)



require(dplyr)
# Capire come funziona il mutate:
head(test)
head(mutate(test, cls=classif))
?aes # --> usato nel ggplot per definire x, y e colore



#################################################################
#################################################################
####### Rappresentiamo graficamente il risultato ottenuto #######
#################################################################
#################################################################
require(ggplot2)
ggplot() +
  # plot dei punti della griglia di test dimensione 1.2 (e classificazione associata)
  geom_point(data = mutate(test, cls=classif),
             size=1.2,
             aes(x=x, y=y, col=cls)) +
  # plot dei punti di train dimensione 3 (e classificazione reale)
  geom_point(data=data.frame(x=train[,1], y=train[,2], cls=cl),
             size=3,
             aes(x=x, y=y, col=cls)) +
  # plot dei punti di train dimensione 3 senza classificazione (è solo per disegnare il bordo nero)
  geom_point(data=data.frame(x=train[,1], y=train[,2], cls=cl),
             size=3,
             shape=1, # setta il bordo nero intorno
             aes(x=x, y=y)) # non settiamo la classe, questo plot è solo per il bordo nero



####################################################################
####################################################################
####### Rappresentiamo graficamente il risultato ottenuto v2 #######
#######            (aggiungiamo le probabilità)              #######
####################################################################
####################################################################

ggplot() +
  # plot dei punti della griglia di test dimensione 1.2 (e classificazione associata)
  # in più rispetto a prima settiamo la "size=prob" in modo da settare 
  # una dimensione del punto proporzionale alla probabilità 
  # del punto (fissata quella classe, ovvero quel colore)
  geom_point(data = mutate(test, cls=classif),
             aes(x=x, y=y, col=cls, size=prob)) +
  scale_size(range=c(0.1, 2)) +
  # plot dei punti di train dimensione 3 (e classificazione reale)
  geom_point(data=data.frame(x=train[,1], y=train[,2], cls=cl),
             size=3,
             aes(x=x, y=y, col=cls)) +
  # plot dei punti di train dimensione 3 senza classificazione (è solo per disegnare il bordo nero)
  geom_point(data=data.frame(x=train[,1], y=train[,2], cls=cl),
             size=3,
             shape=1, # setta il bordo nero intorno
             aes(x=x, y=y)) # non settiamo la classe come colore, questo plot è solo per il bordo nero
