# Esistono parecchi modelli di classificatore bayesiano naif.
# Il modello che abbiamo appena utilizzato si chiama multinomiale, 
# ma esiste anche la versione Bernoulli, adatta per gli indicatori binari 
# (non si contano le parole, ma si verifica se sono presenti) e 
# quella gaussiana (che attende feature normalmente distribuite, 
# con valori sia positivi sia negativi).
# Python, a differenza di R, offre un range completo di classificatori 
# bayesiani naif nel package Scikit-learn all’indirizzo 
# http://scikit-learn.org/stable/modules/naive_bayes.html.

library(klaR)
library(kernlab)
data(spam, package = "kernlab")

View(spam)
set.seed(1234)

train_idx <- sample(1:nrow(spam), ceiling(nrow(spam)*3/4), replace=FALSE)

alpha <- 0
naive <- NaiveBayes(type ~ ., data=spam[train_idx,], prior = c(0.9,0.1), fL = alpha)

library(caret)
predictions <- predict(naive, spam[-train_idx,])
confusionMatrix(predictions$class, spam[-train_idx, "type"])
