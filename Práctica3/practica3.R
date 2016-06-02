## ------------------------------------------------------------------------
x11()
# Función para provocar la parada de la ejecución
parada <- function(){
  cat("\nPresione una tecla para continuar\n")
  t <- readline()
}

## ------------------------------------------------------------------------
# EJERCICIO 1
library(ISLR)

## ------------------------------------------------------------------------
pairs(Auto)

## ------------------------------------------------------------------------
boxplot(Auto$mpg~Auto$cylinders)
parada()
boxplot(Auto$mpg~Auto$horsepower)
parada()
boxplot(Auto$mpg~Auto$weight)
parada()

## ------------------------------------------------------------------------
boxplot(Auto$mpg~Auto$name)
parada()
boxplot(Auto$mpg~Auto$acceleration)
parada()

## ------------------------------------------------------------------------
# Elegimos las cinco primeras columnas de Auto, que contienen mpg, cylinders,
# displacement, horsepower y weight
AutoMod <- Auto[ ,1:5]
# Nos sobra la columna de cylinders, así que la eliminamos
AutoMod <- AutoMod[ ,-2]

## ------------------------------------------------------------------------
# Fijamos la semilla para el sample
set.seed(237)
train = sample(1:nrow(AutoMod), round(nrow(AutoMod)*0.8))
test = AutoMod[-train, ]
train = AutoMod[train, ]

## ------------------------------------------------------------------------
# Obtenemos las posiciones a cambiar con 1 y -1 en train
posiciones <- c(1:length(train[,1]))
pos_positivos <- posiciones[train[,1] >= median(train[,1])]
pos_negativos <- posiciones[train[,1] < median(train[,1])]
train[,1][pos_positivos] = 1
train[,1][pos_negativos] = -1

# Hacemos lo mismo para test
posiciones <- c(1:length(test[,1]))
pos_positivos <- posiciones[test[,1] >= median(test[,1])]
pos_negativos <- posiciones[test[,1] < median(test[,1])]
test[,1][pos_positivos] = 1
test[,1][pos_negativos] = -1

## ------------------------------------------------------------------------
modlog1 <- glm(train$mpg~., data = train)
prediccionesRL <- predict(modlog1, test[,-1])
comp <- sign(prediccionesRL) == sign(test$mpg)
errores <- comp[comp == FALSE]
error.regresion <- 100*(length(errores)/nrow(test))
cat("El error con regresión ha sido:", error.regresion)
parada()

## ------------------------------------------------------------------------
escalado <- scale(train[,2:4])
train[,2:4] <- escalado
centro <- attr(escalado,"scaled:center")
escala <- attr(escalado, "scaled:scale")
test[,2:4] <- scale(test[,2:4], center=centro, scale=escala)

## ------------------------------------------------------------------------
library(class)
library(e1071)
# Pasamos los datos con los que vamos a predecir a una matriz
x <- as.matrix(train[,-1])
# Fijamos la semilla
set.seed(237)
tune.knn(x,as.factor(train[,1]), k=1:10, tunecontrol=tune.control(sampling = "cross"))
# Utilizamos knn con el mejor k que nos ha dicho tune.knn
pred.knn <- knn(train[,-1], test[,-1], train[,1], k=9)


## ------------------------------------------------------------------------
confM <- table(pred.knn, test[,1])
error <- (confM[1,2] + confM[2,1])/(confM[1,1]+confM[1,2]+confM[2,1]+confM[2,2])
cat("El error en test con knn ha sido:", error)
parada()

## ------------------------------------------------------------------------
library(ROCR)
knn.res <- knn(train[,-1], test[,-1], train[,1], k=5, prob=T)
prob <- attr(knn.res,"prob")
prob <- sapply(1:length(prob), function(i) {
  if(as.numeric(knn.res[i]) == 1) {
    1-prob[i]
  }
  else {
    prob[i]
  }
 })

pred <- prediction(prob, test$mpg)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main="Curva ROC para knn")
parada()

## ------------------------------------------------------------------------
pred = prediction(prediccionesRL, test$mpg)
perf = performance(pred, "tpr", "fpr")
plot(perf, main="Curca ROC para regresión logística")
parada()

## ------------------------------------------------------------------------
set.seed(237)
x <- sample(392,392)
l <- c(0, 78, 156, 234, 313, 392)
particiones <- lapply(1:5, function(i) {
    AutoMod[x[(l[i]+1):l[i+1]], ]
})

## ------------------------------------------------------------------------
media_regresion <- 0
media_knn <- 0

for (i in 1:5) {
  test <- particiones[[i]]
  y <- 1:5
  y <- y[y != i]
  train <- rbind(particiones[[y[1]]], particiones[[y[2]]], particiones[[y[3]]],
                 particiones[[y[4]]])
  
  # Discretizamos la variable mpg en train y test
  posiciones <- c(1:length(train[,1]))
  pos_positivos <- posiciones[train[,1] >= median(train[,1])]
  pos_negativos <- posiciones[train[,1] < median(train[,1])]
  train[,1][pos_positivos] = 1
  train[,1][pos_negativos] = -1
  
  posiciones <- c(1:length(test[,1]))
  pos_positivos <- posiciones[test[,1] >= median(test[,1])]
  pos_negativos <- posiciones[test[,1] < median(test[,1])]
  test[,1][pos_positivos] = 1
  test[,1][pos_negativos] = -1
  
  # Calculamos el error con regresión logística
  modlog1 <- glm(train$mpg~., data = train)
  prediccionesRL <- predict(modlog1, test[,-1])
  comp <- sign(prediccionesRL) == sign(test$mpg)
  errores <- comp[comp == FALSE]
  error.regresion <- 100*(length(errores)/nrow(test))
  media_regresion <- media_regresion + error.regresion
  
  # Escalamos para knn
  escalado <- scale(train[,2:4])
  train[,2:4] <- escalado
  centro <- attr(escalado,"scaled:center")
  escala <- attr(escalado, "scaled:scale")
  test[,2:4] <- scale(test[,2:4], center=centro, scale=escala)
  
  # Obtemos el mejor k
  # Pasamos los datos con los que vamos a predecir a una matriz
  x <- as.matrix(train[,-1])
  # Fijamos la semilla
  set.seed(237)
  k <- tune.knn(x, as.factor(train[,1]), k=1:10, tunecontrol = 
                  tune.control(sampling = "cross"))
  k <- k$best.parameters$k
  # Utilizamos knn con el mejor k que nos ha dicho tune.knn
  pred.knn <- knn(train[,-1], test[,-1], train[,1], k=k)
  
  # Creamos la tabla de confusión y calculamos el error
  confM <- table(pred.knn, test[,1])
  error <- (confM[1,2] + confM[2,1]) / 
              (confM[1,1]+confM[1,2]+confM[2,1]+confM[2,2])
  media_knn <- media_knn + error
}

media_regresion <- media_regresion/5
media_knn <- 100*media_knn/5

cat("El error medio por validación cruzada en regresión logística ha sido", 
    media_regresion)
cat("El error medio por validación cruzada con knn ha sido", media_knn)
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(AutoMod, escalado, test, train, x)
rm(centro, comp, confM, error, error.regresion, errores)
rm(escala, knn.res, modlog1, perf, pos_negativos, pos_positivos)
rm(posiciones, pred, pred.knn, prediccionesRL, prob)
rm(particiones, media_regresion, media_knn, l)

## ------------------------------------------------------------------------
# EJERCICIO 2
library(MASS)
set.seed(237)
# Dividimos el conjunto en train (80%) y test(20%)
train = sample(1:nrow(Boston), round(nrow(Boston)*0.8))
test = Boston[-train, ]
train = Boston[train, ]

## ------------------------------------------------------------------------
library(glmnet)
# Volvemos a fijar la semilla
set.seed(237)
modelo.lasso <- glmnet(as.matrix(train[,-1]), as.matrix(train[,1]), alpha=1)
plot(modelo.lasso)
parada()

## ------------------------------------------------------------------------
crossv <- cv.glmnet(as.matrix(train[,-1]), as.matrix(train[,1]), alpha=1)
lambda <- crossv$lambda.min
coeficientes <- predict(modelo.lasso, type="coefficients", s=lambda)[1:14,]
print(coeficientes)
parada()

## ------------------------------------------------------------------------
coeficientes <- coeficientes[abs(coeficientes)>0.2]
print(coeficientes)
parada()

## ------------------------------------------------------------------------
BostonMod.train <- data.frame(train$crim, train$chas, train$nox, train$dis,
                              train$rad)
BostonMod.test <- data.frame(test$crim, test$chas, test$nox, test$dis, test$rad)
modelo.ridge <- glmnet(as.matrix(BostonMod.train[,-1]),
                       as.matrix(BostonMod.train[,1]), alpha=0, lambda=lambda)
predicciones <- predict(modelo.ridge, s=lambda,
                        newx=as.matrix(BostonMod.test[,-1]))

## ------------------------------------------------------------------------
error.res <- sum((BostonMod.test[,1] - predicciones)^2)
error.res <- sqrt(error.res/length(predicciones))
cat("El error residual ha sido:", error.res)
parada()

## ------------------------------------------------------------------------
lambda <- 0.08
modelo.ridge <- glmnet(as.matrix(BostonMod.train[,-1]),
                       as.matrix(BostonMod.train[,1]), alpha=0, lambda=lambda)
predicciones <- predict(modelo.ridge, s=lambda,
                        newx=as.matrix(BostonMod.test[,-1]))
error.res <- sum((BostonMod.test[,1] - predicciones)^2)
error.res <- sqrt(error.res/length(predicciones))
cat("El error para lambda =", lambda, "ha sido", error.res)
parada()

## ------------------------------------------------------------------------
lambda <- 1.5
modelo.ridge <- glmnet(as.matrix(BostonMod.train[,-1]),
                       as.matrix(BostonMod.train[,1]), alpha=0, lambda=lambda)
predicciones <- predict(modelo.ridge, s=lambda,
                        newx=as.matrix(BostonMod.test[,-1]))
error.res <- sum((BostonMod.test[,1] - predicciones)^2)
error.res <- sqrt(error.res/length(predicciones))
cat("El error para lambda =", lambda, "ha sido", error.res)
parada()

## ------------------------------------------------------------------------
lambda <- 0.02
modelo.ridge <- glmnet(as.matrix(BostonMod.train[,-1]),
                       as.matrix(BostonMod.train[,1]), alpha=0, lambda=lambda)
predicciones <- predict(modelo.ridge, s=lambda,
                        newx=as.matrix(BostonMod.test[,-1]))
error.res <- sum((BostonMod.test[,1] - predicciones)^2)
error.res <- sqrt(error.res/length(predicciones))
cat("El error para lambda =", lambda, "ha sido", error.res)
parada()

## ------------------------------------------------------------------------
lambda <- 0.0015
modelo.ridge <- glmnet(as.matrix(BostonMod.train[,-1]),
                       as.matrix(BostonMod.train[,1]), alpha=0, lambda=lambda)
predicciones <- predict(modelo.ridge, s=lambda,
                        newx=as.matrix(BostonMod.test[,-1]))
error.res <- sum((BostonMod.test[,1] - predicciones)^2)
error.res <- sqrt(error.res/length(predicciones))
cat("El error para lambda =", lambda, "ha sido", error.res)
parada()

## ------------------------------------------------------------------------
library(e1071)

## ------------------------------------------------------------------------
posiciones <- c(1:length(train[,1]))
pos_positivos <- posiciones[train[,1] >= median(train[,1])]
pos_negativos <- posiciones[train[,1] < median(train[,1])]
train[,1][pos_positivos] = 1
train[,1][pos_negativos] = -1

posiciones <- c(1:length(test[,1]))
pos_positivos <- posiciones[test[,1] >= median(test[,1])]
pos_negativos <- posiciones[test[,1] < median(test[,1])]
test[,1][pos_positivos] = 1
test[,1][pos_negativos] = -1

## ------------------------------------------------------------------------
# Volvemos a fijar la semilla
set.seed(237)
modelo.svm <- svm(train[,1]~., train[,-1], kernel="linear")
predicciones <- predict(modelo.svm, test[,-1])
comp <- sign(predicciones) == sign(test$crim)
errores <- comp[comp == FALSE]
error <- 100*(length(errores)/nrow(test))
cat("El error con SVM lineal ha sido:", error)
parada()

## ------------------------------------------------------------------------
modelo.svm <- svm(train[,1]~., train[,-1], kernel="polynomial")
predicciones <- predict(modelo.svm, test[,-1])
comp <- sign(predicciones) == sign(test$crim)
errores <- comp[comp == FALSE]
error <- 100*(length(errores)/nrow(test))
cat("El error con SVM polinomial ha sido:", error)
parada()

## ------------------------------------------------------------------------
modelo.svm <- svm(train[,1]~., train[,-1], kernel="radial")
predicciones <- predict(modelo.svm, test[,-1])
comp <- sign(predicciones) == sign(test$crim)
errores <- comp[comp == FALSE]
error <- 100*(length(errores)/nrow(test))
cat("El error con SVM radial ha sido:", error)
parada()

## ------------------------------------------------------------------------
modelo.svm <- svm(train[,1]~., train[,-1], kernel="sigmoid")
predicciones <- predict(modelo.svm, test[,-1])
comp <- sign(predicciones) == sign(test$crim)
errores <- comp[comp == FALSE]
error <- 100*(length(errores)/nrow(test))
cat("El error con SVM sigmoidal ha sido:", error)
parada()

## ------------------------------------------------------------------------
set.seed(237)
x <- sample(405,405)
l <- c(0, 81, 162, 243, 324, 405)
particiones <- lapply(1:5, function(i) {
    Boston[x[(l[i]+1):l[i+1]], ]
})

## ------------------------------------------------------------------------
error_test <- 0
error_train <- 0
for (i in 1:5) {
  test <- particiones[[i]]
  y <- 1:5
  y <- y[y != i]
  train <- rbind(particiones[[y[1]]], particiones[[y[2]]], particiones[[y[3]]],
                 particiones[[y[4]]])
  
  # Discretizamos la variable crim en train y test
  posiciones <- c(1:length(train[,1]))
  pos_positivos <- posiciones[train[,1] >= median(train[,1])]
  pos_negativos <- posiciones[train[,1] < median(train[,1])]
  train[,1][pos_positivos] = 1
  train[,1][pos_negativos] = -1
  
  posiciones <- c(1:length(test[,1]))
  pos_positivos <- posiciones[test[,1] >= median(test[,1])]
  pos_negativos <- posiciones[test[,1] < median(test[,1])]
  test[,1][pos_positivos] = 1
  test[,1][pos_negativos] = -1
  
  modelo.svm <- svm(train[,1]~., train[,-1], kernel="radial")
  predicciones.test <- predict(modelo.svm, test)
  predicciones.train <- predict(modelo.svm, train)
  comp.test <- sign(predicciones.test) == sign(test[,1])
  errores.test <- comp.test[comp.test == FALSE]
  comp.train <- sign(predicciones.train) == sign(train[,1])
  errores.train <- comp.train[comp.train == FALSE]
  error.test <- 100*(length(errores.test)/nrow(test))
  error.train <- 100*(length(errores.train)/nrow(train))
  error_test <- error_test + error.test
  error_train <- error_train + error.train
}

error_train <- error_train/5
error_test <- error_test/5
print("El error con svm de núcelo radial para train con validación cruzada ha sido")
print(error_train)
print("El error con svm de núcelo radial para test con validación cruzada ha sido")
print(error_test)
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(BostonMod.test, BostonMod.train, test, train)
rm(coeficientes, comp, crossv, error, error.res, errores)
rm(lambda, modelo.lasso, modelo.ridge, predicciones, modelo.svm)
rm(pos_negativos, pos_positivos, posiciones, error.test, error.train)
rm(comp.test, comp.train, crim, crim.test, error_test, error_train)
rm(l, particiones, predicciones.test, predicciones.train, y)
rm(errores.test, errores.train)

## ------------------------------------------------------------------------
# EJERCICIO 3
library(randomForest)
library(gbm)
library(MASS)

## ------------------------------------------------------------------------
set.seed(237)
train = sample(1:nrow(Boston), round(nrow(Boston)*0.8))
test = Boston[-train, ]
train = Boston[train, ]

## ------------------------------------------------------------------------
summary(train)

## ------------------------------------------------------------------------
bagging10 <- randomForest(train$medv~., data = train, mtry = 13, ntree = 100)
pred10 <- predict(bagging10, newdata = test)
cat("El error con 10 árboles es:", mean((pred10-test$medv)^2))
parada()

## ------------------------------------------------------------------------
bagging300 <- randomForest(train$medv~., data = train, mtry = 13, ntree = 300)
pred300 <- predict(bagging300, newdata = test)
cat("El error con 300 árboles es:", mean((pred300-test$medv)^2))
parada()

## ------------------------------------------------------------------------
bagging500 <- randomForest(train$medv~., data = train, mtry = 13, ntree = 500)
pred500 <- predict(bagging500, newdata = test)
cat("El error con 500 árboles es:", mean((pred500-test$medv)^2))
parada()

## ------------------------------------------------------------------------
bagging600 <- randomForest(train$medv~., data = train, mtry = 13, ntree = 600)
pred600 <- predict(bagging600, newdata = test)
cat("El error con 600 árboles es:", mean((pred600-test$medv)^2))
parada()

## ------------------------------------------------------------------------
bagging700 <- randomForest(train$medv~., data = train, mtry = 13, ntree = 700)
pred700 <- predict(bagging700, newdata = test)
cat("El error con 700 árboles es:", mean((pred700-test$medv)^2))
parada()

## ------------------------------------------------------------------------
bagging650 <- randomForest(train$medv~., data = train, mtry = 13, ntree = 650)
pred650 <- predict(bagging650, newdata = test)
cat("El error con 650 árboles es:", mean((pred650-test$medv)^2))
parada()

## ------------------------------------------------------------------------
# Fijamos la semilla de nuevo
set.seed(237)
randomF10 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 10)
pred10 <- predict(randomF10, newdata=test)
cat("El error con 10 árboles es:", mean((pred10-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF30 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 30)
pred30 <- predict(randomF30, newdata=test)
cat("El error con 30 árboles es:", mean((pred30-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF50 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 50)
pred50 <- predict(randomF50, newdata=test)
cat("El error con 50 árboles es:", mean((pred50-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF100 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 100)
pred100 <- predict(randomF100, newdata=test)
cat("El error con 100 árboles es:", mean((pred100-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF200 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 200)
pred200 <- predict(randomF200, newdata=test)
cat("El error con 200 árboles es:", mean((pred200-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF400 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 400)
pred400 <- predict(randomF400, newdata=test)
cat("El error con 400 árboles es:", mean((pred400-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF600 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 600)
pred600 <- predict(randomF600, newdata=test)
cat("El error con 600 árboles es:", mean((pred600-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF500 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 500)
pred500 <- predict(randomF500, newdata=test)
cat("El error con 100 árboles es:", mean((pred500-test$medv)^2))
parada()

## ------------------------------------------------------------------------
randomF450 <- randomForest(train$medv~., data = train, mtry = 5, ntree = 450)
pred450 <- predict(randomF450, newdata=test)
cat("El error con 450 árboles es:", mean((pred450-test$medv)^2))
parada()

## ------------------------------------------------------------------------
library(gbm)
set.seed(237)

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 400, shrinkage = 0.001)
pred <- predict(boosting, newdata = test, n.trees=400)
print("Error para 400 árboles y shrinkage = 0.001")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 400, shrinkage = 0.01)
pred <- predict(boosting, newdata = test, n.trees=400)
print("Error para 400 árboles y shrinkage = 0.01")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 500, shrinkage = 0.01)
pred <- predict(boosting, newdata = test, n.trees=500)
print("Error para 500 árboles y shrinkage = 0.01")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 500, shrinkage = 0.1)
pred <- predict(boosting, newdata = test, n.trees=500)
print("Error para 500 árboles y shrinkage = 0.1")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 600, shrinkage = 0.1)
pred <- predict(boosting, newdata = test, n.trees=600)
print("Error para 500 árboles y shrinkage = 0.1")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 600, shrinkage = 0.2)
pred <- predict(boosting, newdata = test, n.trees=600)
print("Error para 500 árboles y shrinkage = 0.1")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
boosting <- gbm(medv~., data = train, distribution = "gaussian", n.trees = 700, shrinkage = 0.1)
pred <- predict(boosting, newdata = test, n.trees=700)
print("Error para 500 árboles y shrinkage = 0.1")
print(mean((pred - test$medv)^2))
parada()

## ------------------------------------------------------------------------
# EJERCICIO 4
# Fijamos de nuevo la semilla
set.seed(237)

library(ISLR)
train.idx = sample(1:nrow(OJ), 800)
test = OJ[-train.idx, ]
train = OJ[train.idx, ]

# Usamos la librería tree
library(tree)

## ------------------------------------------------------------------------
summary(OJ)

## ------------------------------------------------------------------------
tree.oj <- tree(train$Purchase~., train[,-1])

## ------------------------------------------------------------------------
plot(tree.oj)
text(tree.oj, pretty = 0)
parada()

## ------------------------------------------------------------------------
summary(tree.oj)
parada()

## ------------------------------------------------------------------------
tree.predict <- predict(tree.oj, test[,-1], type="class")
table(tree.predict, test[,1])
parada()

## ------------------------------------------------------------------------
set.seed(237)
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
print(cv.oj)
parada()

## ------------------------------------------------------------------------
plot(cv.oj$size, cv.oj$dev, type = "b", main = "CV para tamaño de árbol",
     ylab = "Error en validación cruzada", xlab = "Número de nodos")
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(test, train, cv.oj, train.idx, tree.oj, tree.predict)

