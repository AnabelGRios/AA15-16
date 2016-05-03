## ------------------------------------------------------------------------
set.seed(237)
x11()
######
# Funciones de la primera práctica
######

# Simular números aleatorios uniformes
simula_unif = function (N=2, dims=2, rango = c(0,1)){
 matrix(runif(N*dims, min=rango[1], max=rango[2]), nrow = N, ncol=dims, byrow=T)
}

# Simular números aleatorios en una normal
simula_gauss <- function(N, dim, sigma) {
  lapply(1:N, function(x) rnorm(dim, mean = 0, sqrt(sigma)))
}

# Simular recta que corte a un intervalo dado
simula_recta <- function(intervalo) {
  m <- simula_unif(2, 2, intervalo)
  a <- (m[2,2] - m[1,2]) / (m[2,1] - m[1,1])
  b <- m[1,2] - a * m[1,1]
  c(a,b)
}

# Calcular la simetría de una matriz
calcular_simetria <- function(mat) {
  # Invertimos la matriz por columnas
  mat_invertida = apply(mat, 2, function(x) rev(x))
  # Calulamos el valor absoluto de la diferencia de cada elemento entre las dos
  # matrices
  dif = abs(mat - mat_invertida)
  # Sumamos los elementos de la matriz
  suma <- sum(dif)
  # Devolvemos el signo cambiado de la suma
  -suma
}


## ------------------------------------------------------------------------
# Método de regresión lineal
Regress_Lin <- function(datos, label) {
  descomp <- La.svd(datos)
  vt <- descomp[[3]]
  # Creamos la inversa de la matriz diagonal al cuadrado
  diagonal <- matrix(0, length(descomp[[1]]), length(descomp[[1]]))
  for (i in 1:length(descomp[[1]])) {
    diagonal[i,i] = descomp[[1]][i]
    if (diagonal[i,i] != 0) {
      diagonal[i,i] = 1/(diagonal[i,i]^2)
    }
  }
  prod_inv <- t(vt) %*% diagonal %*% vt
  pseud_inv <- prod_inv %*% t(datos)
  w <- pseud_inv %*% label
  return(w)
}

# Función para contar diferencias dados dos vectores necesaria en la siguiente
# función
cuenta_diferencias <- function(etiquetas1, etiquetas2) {
  vf <- etiquetas1 == etiquetas2
  length(vf[vf == FALSE])
}

# Función para contar errores necesaria en el PLA pocket
cuenta_errores <- function(w, etiquetas_originales, datos) {
  # Etiquetamos con la solución del PLA
  etiquetas_cambiadas <- unlist(lapply(1:nrow(datos), function(i) {
    # Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos[i,]
    f <- crossprod(w,p)
    sign(f)
  }))
  # Devolvemos el número de errores que da la solución
  cuenta_diferencias(etiquetas_originales, etiquetas_cambiadas)
}

# Algoritmo PLA pocket
ajusta_PLA_MOD <- function(datos, label, max_iter, vini) {
  parada <- F
  fin <- F
  w <- vini
  wmejor <- w
  iter <- 1
  errores_mejor <- cuenta_errores(wmejor, label, datos)
  # Mientras no hayamos superado el máximo de iteraciones o
  # no se haya encontrado solución
  while(!parada) {
    # iteramos sobre los datos
    for (j in 1:nrow(datos)) {
      if (sign(crossprod(w, datos[j,])) != label[j]) {
        w <- w + label[j]*datos[j,]
        # La variable fin controla si se ha entrado en el if
        fin <- F
      }
    }
    # Contamos el número de errores que hay en la solución actual y si
    # es menor que el número de errores en la mejor solución de las que
    # llevamos, nos quedamos con la actual
    errores_actual <- cuenta_errores(w, label, datos)
    if(errores_actual < errores_mejor) {
      wmejor <- w
      errores_mejor <- errores_actual
    }
    # Si no se ha entrado en el if, todos los datos estaban bien
    # clasificados y podemos poner a TRUE la variable parada.
    if(fin == T) {
      parada = T
    }
    else {
      fin = T
    }
    iter <- iter + 1
    if (iter >= max_iter) parada = T
  }
  
  # Devolvemos el hiperplano, el número máximo de iteraciones al que hemos
  # llegado y el número de errores de la mejor solución que hemos encontrado
  list(w = wmejor, numIteraciones = iter, errores = errores_mejor)
}


## ------------------------------------------------------------------------
# Función para pintar datos junto con gráficas
pinta_particion <- function(coordX, coordY, etiquetas=NULL, visible=FALSE, 
                            f=NULL, main="", xlab="Eje X", ylab="Eje Y",
                            rango=1) {
  if(is.null(etiquetas))
    etiquetas=1
  else etiquetas = etiquetas+3
  
  plot(coordX, coordY, type = "p", col = etiquetas, xlab = xlab, ylab = ylab, 
       main = main)
  
  # Si queremos pintar función junto con los datos
  if(visible) {
    sec <- seq(-rango, rango, length.out = 1500)
    z <- outer(seq(-1,1,length.out=1000), sec, f)
    contour(seq(-1,1,length.out=1000), sec, z, col = "blue", levels = 0, add = TRUE, drawlabels = F)
  }
}

## ------------------------------------------------------------------------
# Función para provocar la parada de la ejecución
parada <- function(){
  cat("\nPresione una tecla para continuar\n")
  t <- readline()
}

## ------------------------------------------------------------------------
###########
cat("EJERCICIO 1\n")
###########

## ------------------------------------------------------------------------
cat("APARTADO 1: GRADIENTE DESCENDENTE\n")

# Algoritmo del gradiente descendente. Le pasamos a la función la función de
# error, su gradiente (que serán funciones), el punto en el que se empieza, la
# tasa de aprendizaje, el número máximo de iteraciones a realizar, y el mínimo 
# error al que queremos llegar, en orden.
# Devuelve los valores de la función de error por los que pasa junto con la
# iteración.
gradienteDescendente <- function(ferror, gradiente, pini, tasa, maxiter, umbral) {
  w <- pini
  i <- 1
  valoresError <- c(i, ferror(pini[1], pini[2]))
  mejora <- TRUE
  while (i <= maxiter && mejora) {
    g <- gradiente(w[1], w[2])
    # Le cambiamos la dirección al gradiente para ir hacia abajo
    v <- -g
    # Nos movemos tanto como indique la tasa
    wnew <- w + tasa*v
    valoresError <- rbind(valoresError, c(i, ferror(wnew[1], wnew[2])))
    
    if (abs(ferror(wnew[1], wnew[2])-ferror(w[1], w[2])) < umbral || 
        ferror(wnew[1], wnew[2]) < umbral || i==maxiter) {
      mejora <- FALSE
      cat("He necesitado", i, "iteraciones para llegar al error", 
          ferror(wnew[1], wnew[2]),"\n")
      cat("con valores de u y v:", wnew[1],",", wnew[2])
      mostrar <- FALSE
    }
    w <- wnew
    i <- i+1
  }
  return(valoresError)
}


## ------------------------------------------------------------------------
E <- function(u,v) (u*exp(v) - 2*v*exp(-u))^2
gradE <- function(u,v) {(2*(u*exp(v) - 2*v*exp(-u)))*c(exp(v) + 2*v*exp(-u),
                                                       u*exp(v) - 2*exp(-u))}

val <- gradienteDescendente(E, gradE, c(1,1), 0.1, 20, 10^{-14})
parada()

## ------------------------------------------------------------------------
f <- function(x,y) x^2 + 2*y^2 + 2*sin(2*pi*x)*sin(2*pi*y)
gradF <- function(x,y) c(2*x + 4*pi*sin(2*pi*y)*cos(2*pi*x),
                         4*y + 4*pi*sin(2*pi*x)*cos(2*pi*y))

val <- gradienteDescendente(f, gradF, c(1,1), 0.01, 50, 0)
parada()
pinta_particion(val[,1], val[,2], xlab="num iteración", ylab="f(x)", 
                main="Gradiente Descendente")
parada()

## ------------------------------------------------------------------------
val <- gradienteDescendente(f, gradF, c(1,1), 0.1, 50, 0)
pinta_particion(val[,1], val[,2], xlab="num iteración", ylab="f(x)", 
                main="Gradiente Descendente")
parada

## ------------------------------------------------------------------------
cat("\nPunto de inicio (0.1,0.1)\n")
val <- gradienteDescendente(f, gradF, c(0.1,0.1), 0.01, 50, 0)
parada()
cat("Punto de inicio (1,1)\n")
val <- gradienteDescendente(f, gradF, c(1,1), 0.01, 50, 0)
parada()
cat("Punto de inicio (-0.5,-0.5)\n")
val <- gradienteDescendente(f, gradF, c(-0.5,-0.5), 0.01, 50, 0)
parada()
cat("Punto de inicio (-1,-1)\n")
val <- gradienteDescendente(f, gradF, c(-1,-1), 0.01, 50, 0)
parada()

## ------------------------------------------------------------------------
cat("APARTADO 2: COORDENADA DESCENDENTE\n")

# Algoritmo de coordenada descendente. Le pasamos a la función la función de
# error, su gradiente (que serán funciones), el punto en el que se empieza, la
# tasa de aprendizaje, el número máximo de iteraciones a realizar, y el mínimo 
# error al que queremos llegar, en orden.
coordenadaDescendente <- function(ferror, gradiente, pini, tasa, maxiter, umbral) {
  w <- pini
  i <- 1
  mejora <- TRUE
  while (i <= maxiter && mejora) {
    # Paso 1
    g <- gradiente(w[1], w[2])
    # Le cambiamos la dirección al gradiente para ir hacia abajo
    v <- -g
    wnew <- w
    wnew[1] <- w[1] + tasa*v[1]
    
    # Paso 2
    g <- gradiente(wnew[1], wnew[2])
    # Le cambiamos la dirección al gradiente para ir hacia abajo
    v <- -g
    wnew[2] <- w[2] + tasa*v[2]
    
    if (abs(ferror(wnew[1],wnew[2]) - ferror(w[1],w[2])) < umbral && 
        ferror(wnew[1],wnew[2]) < umbral || i==maxiter) {
      mejora <- FALSE
      cat("He necesitado", i, "iteraciones para llegar al error", 
          ferror(wnew[1], wnew[2]),"\n")
      cat("con valores de u y v:", w[1],",", w[2])
    }
    
    w <- wnew
    i <- i+1
  }
}


## ------------------------------------------------------------------------
val <- coordenadaDescendente(E, gradE, c(1,1), 0.1, 15, 0)
parada()

## ------------------------------------------------------------------------
cat("APARTADO 3: MÉTODO DE NEWTON\n")

# Algoritmo del método de Newton. Le pasamos a la función la función de
# error, su gradiente y la matriz hessiana (que serán funciones), el punto 
# en el que se empieza, la tasa de aprendizaje, el número máximo de iteraciones
# a realizar, y el mínimo error al que queremos llegar, en orden.
# Devuelve los valores de la función de error por los que pasa junto con la
# iteración.
metodoNewton <- function(ferror, gradiente, hessiana, pini, tasa, maxiter, umbral) {
  w <- pini
  i <- 1
  valoresError <- c(i, ferror(pini[1], pini[2]))
  mejora <- TRUE
  while (i <= maxiter && mejora) {
    hg <- solve(hessiana(w[1], w[2]))%*%gradiente(w[1], w[2])
    # Le cambiamos la dirección a la hessiana por el gradiente para ir hacia abajo
    v <- -hg
    # Nos movemos tanto como indique la tasa
    wnew <- w + tasa*v
    # Vamos guardando los valores de error por los que vamos pasando
    valoresError <- rbind(valoresError, c(i, ferror(wnew[1], wnew[2])))
    
    if (abs(ferror(wnew[1], wnew[2]) - ferror(w[1], w[2])) < umbral ||
        ferror(wnew[1], wnew[2]) < umbral || i==maxiter) {
      mejora <- FALSE
      cat("He necesitado", i, "iteraciones para llegar al error", 
          ferror(wnew[1], wnew[2]),"\n")
      cat("con valores de u y v:", wnew[1],",", wnew[2])
    }
    
    w <- wnew
    i <- i+1
  }
  return(valoresError)
}

## ------------------------------------------------------------------------
# Cálculo de f, el gradiente y la hessiana
f <- function(x,y) x^2 + 2*y^2 + 2*sin(2*pi*x)*sin(2*pi*y)
gradF <- function(x,y) c(2*x + 4*pi*sin(2*pi*y)*cos(2*pi*x),
                         4*y + 4*pi*sin(2*pi*x)*cos(2*pi*y))
d12 <- function(x,y) 8*pi^2*cos(2*pi*y)*cos(2*pi*x)
d11 <- function(x,y) 2 - 8*pi^2*sin(2*pi*y)*sin(2*pi*x)
d22 <- function(x,y) 4 - 8*pi^2*sin(2*pi*x)*sin(2*pi*y)

hess <- function(x,y) rbind(c(d11(x,y), d12(x,y)), c(d12(x,y), d22(x,y)))

val <- metodoNewton(f, gradF, hess, c(1,1), 0.01, 50, 0)
parada()
val2 <- metodoNewton(f, gradF, hess, c(1,1), 0.1, 50, 0)
parada()

## ------------------------------------------------------------------------
pinta_particion(val[,1], val[,2], xlab="num iteración", ylab="f(x)",
                main="Método Newton con tasa 0.01")
parada()

## ------------------------------------------------------------------------
pinta_particion(val2[,1], val2[,2], xlab="num iteración", ylab="f(x)",
                main="Método Newton con tasa 0.1")
parada()

## ------------------------------------------------------------------------
cat("APARTADO 4: REGRESIÓN LOGÍSTICA\n")

muestra <- simula_unif(100, 2, c(-1,1))

## ------------------------------------------------------------------------
recta <- simula_recta(c(-1,1))

## ------------------------------------------------------------------------
etiquetas <- unlist(lapply(1:nrow(muestra), function(i) {
  p <- muestra[i,]
  sign(p[2] - recta[1]*p[1] - recta[2])
}))

## ------------------------------------------------------------------------
# Función para calcular la norma euclídea de un vector
calcularNorma <- function(vec) {
  sqrt(sum(vec^2))
}

## ------------------------------------------------------------------------
# Algoritmo RL con SGD. Recibe los datos en una matriz, las etiquetas de esos
# datos, el vector inicial, la tasa de aprendizaje, el máximo número de 
# iteraciones y el umbral para la condición de parada.
Regress_LogSGD <- function(datos, label, vini, tasa, maxiter, umbral) {
  parada <- F
  w <- vini
  iter <- 0
  # Mientras no hayamos superado el máximo de iteraciones o
  # no se hayan acercado lo suficiente w y wnew
  while(!parada && iter < maxiter) {
    # Hacemos una permutación al orden en el que vamos a utilizar los datos
    pos <- sample(1:nrow(datos), nrow(datos))
    # iteramos sobre los datos
    wold <- w
    for (j in pos) {
      grad <- (-label[j]*datos[j,])/(1+exp(label[j]*crossprod(w, datos[j,])))
      w <- w - tasa*grad
    }
    if(calcularNorma(wold-w) < umbral) { 
      parada <- TRUE
    }
    iter <- iter+1
  }
  
  # Devolvemos los pesos finales
  return(list(pesos=w, iteraciones=iter))
}

## ------------------------------------------------------------------------
cat("Solución con regresión logística\n")
sol <- Regress_LogSGD(cbind(muestra,1), etiquetas, c(0,0,0), 0.01, 400, 0.01)
sol
parada()

g <- -sol[[1]]/sol[[1]][2]
muestra_out <- simula_unif(1000, 2, c(-1,1))
# Etiquetamos con la función original
etiquetas_originales <- unlist(lapply(1:nrow(muestra_out), function(i) {
  p <- muestra_out[i,]
  sign(p[2] - recta[1]*p[1] - recta[2])
}))
# Etiquetamos con la g estimada
etiquetas_g <- unlist(lapply(1:nrow(muestra_out), function(i) {
  p <- muestra_out[i,]
  sign(p[2] - g[1]*p[1] - g[3])
}))
# Contamos los errores
errores <- cuenta_diferencias(etiquetas_originales, etiquetas_g)
cat("El error Eout es: ", 100*(errores/nrow(muestra)))
parada()

## ------------------------------------------------------------------------
cat("\nNota: este apartado tarda un poco en ejecutarse\n")
errores <- 0
iteraciones <- 0
for(i in 1:100) {
  # Generamos una nueva recta que separe
  recta_i <- simula_recta(c(-1,1))
  # Etiquetamos con esta recta
  etiquetas_i <- unlist(lapply(1:nrow(muestra), function(i) {
    p <- muestra[i,]
    sign(p[2] - recta_i[1]*p[1] - recta_i[2])
  }))
  
  # Estimamos g
  sol <- Regress_LogSGD(cbind(muestra,1), etiquetas_i, c(0,0,0), 0.01, 500, 0.01)
  g <- -sol[[1]]/sol[[1]][2]
  # Acumulamos las iteraciones que tarda en converger
  iteraciones <- iteraciones + sol[[2]]
  
  # Calculamos las etiquetas originales y las estimadas fuera de la muestra
  etiquetas_originales <- unlist(lapply(1:nrow(muestra_out), function(i) {
    p <- muestra_out[i,]
    sign(p[2] - recta_i[1]*p[1] - recta_i[2])
  }))
  etiquetas_g <- unlist(lapply(1:nrow(muestra_out), function(i) {
    p <- muestra_out[i,]
    sign(p[2] - g[1]*p[1] - g[3])
  }))
  # Contamos errores y acumulamos
  errores <- errores + cuenta_diferencias(etiquetas_originales, etiquetas_g)
}

cat("\nEl número medio de Eout ha sido:", errores/100)
parada()

## ------------------------------------------------------------------------
cat("\nRL tarda en converger", iteraciones/100, "iteraciones en promedio")
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(muestra, muestra_out)
rm(errores, iteraciones)
rm(etiquetas, etiquetas_g, etiquetas_originales)
rm(g, i, recta, recta_i)
rm(val, val2, etiquetas_i, sol, hess)
rm(d11, d12, d22, E, f, gradE, gradF)

## ------------------------------------------------------------------------
cat("APARTADO 5: CLASIFICACIÓN DE DÍGITOS\n")

# Leemos los ficheros y extraemos los dígitos 1 y 5
train <- read.table("datos/zip.train", sep=" ")
test <- read.table("datos/zip.test", sep=" ")
numero_train <- train$V1
numero_test <- test$V1
frame_train <- train[numero_train==1 | numero_train==5,]
frame_test <- test[numero_test==1 | numero_test==5,]
numero_train <- numero_train[numero_train==1 | numero_train==5]
numero_test <- numero_test[numero_test==1 | numero_test==5]

# Hacemos los vectores de etiquetas
etiquetas_train = numero_train
etiquetas_train[numero_train==5] = -1
etiquetas_test = numero_test
etiquetas_test[numero_test==5] = -1

# Eliminamos de cada uno la primera columna, que guarda el número del
# que son los datos, y la última, que tiene NA
frame_train = frame_train[,-258]
frame_train = frame_train[,-1]
frame_test = frame_test[,-258]
frame_test = frame_test[,-1]

# Los pasamos a matrices
frame_train <- data.matrix(frame_train)
frame_test <- data.matrix(frame_test)

# Hacemos una lista de matrices
lista_train <- lapply(split(frame_train, seq(nrow(frame_train))), function(x) {
  matrix(x, 16, 16, T)
})
lista_test <- lapply(split(frame_test, seq(nrow(frame_test))), function(x) {
  matrix(x, 16, 16, T)
})

# Eliminamos lo que no vamos a utilizar
rm(train)
rm(test)
rm(frame_train)
rm(frame_test)
rm(numero_train)
rm(numero_test)

## ------------------------------------------------------------------------
# Calculamos primero las intensidades y las simetrías de todas las matrices,
# es decir, de todas las instancias de 1's y 5's
train_simetria <- unlist(lapply(lista_train, function(m) calcular_simetria(m)))
train_intensidad <- unlist(lapply(lista_train, function(m) mean(m)))

test_simetria <- unlist(lapply(lista_test, function(m) calcular_simetria(m)))
test_intensidad <- unlist(lapply(lista_test, function(m) mean(m)))

## ------------------------------------------------------------------------
datos <- cbind(train_intensidad, train_simetria, 1)
w <- Regress_Lin(datos, etiquetas_train)
cat("\nSolución con regresión lineal:", w)
# Le pasamos lo que nos devuelve la regresión lineal al PLA pocket como solución
# inicial
sol <- ajusta_PLA_MOD(datos, etiquetas_train, 100, w)
cat("\nSolución después de PLA pocket:", sol[[1]])
parada()

## ------------------------------------------------------------------------
# Obtenemos la solución devuelta por el PLA
r <- sol[[1]]
r <- -r/r[2]
plot(train_intensidad, train_simetria, col=etiquetas_train+3, type="p", 
     main="Conjunto TRAIN", xlab="Intensidad", ylab="Simetría")
abline(r[3],r[1])
parada()

## ------------------------------------------------------------------------
plot(test_intensidad, test_simetria, type="p", col=etiquetas_test+3,
     main="Conjunto TEST", xlab="Intensidad", ylab="Simetría")
abline(r[3], r[1])
parada()

## ------------------------------------------------------------------------
etiquetas_g_in <- unlist(lapply(1:nrow(datos), function(i) {
  p <- datos[i,]
  sign(p[2] - r[1]*p[1] - r[3])
}))
datos_test <- cbind(test_intensidad, test_simetria, 1)
etiquetas_g_test <- unlist(lapply(1:nrow(datos_test), function(i) {
  p <- datos_test[i,]
  sign(p[2] - r[1]*p[1] - r[3])
}))
Ein <- 100*cuenta_diferencias(etiquetas_train, etiquetas_g_in)/nrow(datos)
Etest <- 100*cuenta_diferencias(etiquetas_test, etiquetas_g_test)/nrow(datos_test)
cat("\nEl error en la muestra Ein ha sido", Ein)
cat("\nEl error en el test Etest ha sido", Etest)
parada()

## ------------------------------------------------------------------------
# Cota sobre la función de crecimiento
mH_cota <- (2*nrow(datos))^3+1
# Cota basada en Ein
Eout_cota1 <- Ein + sqrt((8/nrow(datos))*log((4*mH_cota)/0.05))
cat("\nLa cota basada en Ein es", Eout_cota1)
parada()

## ------------------------------------------------------------------------
x1 <- train_intensidad
x2 <- train_simetria
datos <- cbind(1, x1, x2, x1^2, x2^2, x1*x2, x1^3, x2^3, x1*x2^2, x2*x1^2)

# Hacemos regresión lineal
w <- Regress_Lin(datos, etiquetas_train)
# Le pasamos el PLA
g_pol <- ajusta_PLA_MOD(datos, etiquetas_train, 100, w)[[1]]

etiquetas_g_in <- unlist(lapply(1:nrow(datos), function(i) {
  p <- datos[i,]
  sign(g_pol[1]+g_pol[2]*p[2]+g_pol[3]*p[3]+g_pol[4]*p[4]+g_pol[5]*p[5]+
         g_pol[6]*p[6]+g_pol[7]*p[7]+g_pol[8]*p[8]+g_pol[9]*p[9]+
         g_pol[10]*p[10])
}))

x1 <- test_intensidad
x2 <- test_simetria
datos_test <- cbind(1, x1, x2, x1^2, x2^2, x1*x2, x1^3, x2^3, x1*x2^2, x2*x1^2)

etiquetas_g_test <- unlist(lapply(1:nrow(datos_test), function(i) {
  p <- datos_test[i,]
 sign(g_pol[1]+g_pol[2]*p[2]+g_pol[3]*p[3]+g_pol[4]*p[4]+g_pol[5]*p[5]+
         g_pol[6]*p[6]+g_pol[7]*p[7]+g_pol[8]*p[8]+g_pol[9]*p[9]+
         g_pol[10]*p[10])
}))

Ein <- 100*cuenta_diferencias(etiquetas_train, etiquetas_g_in)/nrow(datos)
Etest <- 100*cuenta_diferencias(etiquetas_test, etiquetas_g_test)/nrow(datos_test)
cat("\nEl error en la muestra Ein ha sido", Ein)
cat("\nEl error en el test Etest ha sido", Etest)
parada()

## ------------------------------------------------------------------------
# Pintamos el conjunto de train
funcion <- function(x,y) g_pol[1]+g_pol[2]*x+g_pol[3]*y+g_pol[4]*x^2+
  g_pol[5]*y^2+g_pol[6]*x*y+g_pol[7]*x^3+g_pol[8]*y^3+g_pol[9]*x*y^2+
  g_pol[10]*y*x^2
pinta_particion(train_intensidad, train_simetria, etiquetas_train, T, f=funcion,
                main="Conjunto TRAIN con transformación polinómica", 
                xlab="Intensidad", ylab="Simetría", rango=250)
parada()

## ------------------------------------------------------------------------
# Pintamos el conjunto de test
pinta_particion(test_intensidad, test_simetria, etiquetas_test, T, f=funcion,
                main="Conjunto TEST con transformación polinómica", 
                xlab="Intensidad", ylab="Simetría", rango=250)
parada()

## ------------------------------------------------------------------------
# Cota sobre la función de crecimiento
mH_cota <- (2*nrow(datos))^3+1
# Cota basada en Ein
Eout_cota1 <- Ein + sqrt((8/nrow(datos))*log((4*mH_cota)/0.05))
cat("\nLa cota basada en Ein es", Eout_cota1)
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(datos, datos_test, r, w)
rm(Ein, Eout_cota1, Etest, etiquetas_g_in)
rm(etiquetas_g_test, etiquetas_test, etiquetas_train)
rm(lista_test, lista_train, mH_cota, sol, test_intensidad)
rm(test_simetria, train_intensidad, train_simetria, x1, x2)
rm(g_pol)

## ------------------------------------------------------------------------
#############
cat("EJERCICIO 2\n")
#############

## ------------------------------------------------------------------------
cat("APARTADO 1\n")

# Reinicializamos la semilla
set.seed(27172)
# Fijamos parámetros y extraemos los aq
N <- 10
sigma <- 0.2^2
Qf <- 3
aq <- unlist(simula_gauss(Qf+1, 1, sigma))
# Escalamos
escalado <- sapply(0:Qf, function(q) 1/(2*q+1))
escalado <- sum(sqrt(escalado))
aq <- aq/escalado

## ------------------------------------------------------------------------
# Función para calcular los polinomios de Legendre, con una ecuación en
# diferencias, de hasta grado k en función del valor en el que evaluemos x.
polLegendre <- function(x, k) {
  L <- vector("numeric", k+1)
  if(k == 0) {
    L[1] <- 1
  }
  else {
    L[1] <- 1
    L[2] <- x
    if (k > 2){
      for(i in 2:k) {
        L[i+1] <- ((2*i-1)/i)*x*L[i] - ((i-1)/i)*L[i-1]
      }
    }
  }
  return(L)
}

## ------------------------------------------------------------------------
f <- function(x, aq, Qf) {
  sapply(x, function(x_i){
    crossprod(aq, polLegendre(x_i,Qf))
  })
}

## ------------------------------------------------------------------------
datos <- unlist(simula_unif(N, 1, c(-1,1)))
epsilon_n <- unlist(simula_gauss(N, 1, 1))
y_n <- f(datos, aq, Qf) + sqrt(sigma)*epsilon_n

## ------------------------------------------------------------------------
w <- Regress_Lin(cbind(1, datos, datos^2), y_n)
# Pintamos el resultado
pinta_particion(datos, y_n, etiquetas=NULL, visible=T, 
                            function(x,y) w[1]+w[2]*x+w[3]*x^2-y, 
                main="Polinomio de orden 2", xlab="Eje X", ylab="Eje Y")
parada()

## ------------------------------------------------------------------------
w10 <- Regress_Lin(cbind(1, datos, datos^2, datos^3, datos^4, datos^5, datos^6, 
                         datos^7, datos^8, datos^9, datos^10), y_n)
pinta_particion(datos, y_n, etiquetas=NULL, visible=T, 
                            function(x,y) w10[1]+w10[2]*x+w10[3]*x^2+w10[4]*x^3+
                  w10[5]*x^4+w10[6]*x^5+w10[7]*x^6+w10[8]*x^7+w10[9]*x^8+
                  w10[10]*x^9+w10[11]*x^10-y, main="Polinomio de orden 10",
                xlab="Eje X", ylab="Eje Y")
parada()

## ------------------------------------------------------------------------
cat("APARTADO 2\n")

Qf <- 20
N <- 50
sigma <- 1
aq <- unlist(simula_gauss(Qf+1, 1, sigma))
# Escalamos
escalado <- sapply(0:Qf, function(q) 1/(2*q+1))
escalado <- sum(sqrt(escalado))
aq <- aq/escalado
datos <- unlist(simula_unif(N, 1, c(-1,1)))
epsilon_n <- unlist(simula_gauss(N, 1, 1))
y_n <- f(datos, aq, Qf) + sqrt(sigma)*epsilon_n

# Obtenemos g2 y g10 con estos datos
w2 <- Regress_Lin(cbind(1, datos, datos^2), y_n)
w10 <- Regress_Lin(cbind(1, datos, datos^2, datos^3, datos^4, datos^5, datos^6, 
                         datos^7, datos^8, datos^9, datos^10), y_n)

## ------------------------------------------------------------------------
error2 <- 0
error10 <- 0
for(i in 0:100) {
  # Generamos 100 datos fuera de la muestra inicial
  datos_out <- unlist(simula_unif(100, 1, c(-1,1)))
  
  for(j in 1:100) {
    # Calculamos lo que vale cada punto en realidad con la función objetivo
    valor_real <- f(datos_out[j], aq, Qf)
    # Calculamos ahora la diferencia al cuadrado de las diferencias con los 
    # polinomios de grado 2 y grado 10 y las acumulamos
    valorg2 <- w2[1]+w2[2]*datos_out[j]+w2[3]*datos_out[j]^2
    valorg10 <- w10[1]+w10[2]*datos_out[j]+w10[3]*datos_out[j]^2+
      w10[4]*datos_out[j]^3+w10[5]*datos_out[j]^4+w10[6]*datos_out[j]^5+
      w10[7]*datos_out[j]^6+w10[8]*datos_out[j]^7+w10[9]*datos_out[j]^8+
      w10[10]*datos_out[j]^9+w10[11]*datos_out[j]^10
    error2 <- error2 + (valor_real - valorg2)^2
    error10 <- error10 + (valor_real - valorg10)^2
  }
}
Eout2 <- error2/100
Eout10 <- error10/100

cat("\nEout para el polinomio de grado 2 ha sido", Eout2)
cat("\nEout para el polinomio de grado 10 ha sido", Eout10)
parada()

## ------------------------------------------------------------------------
sobreajuste <- Eout10 - Eout2
cat("\nLa medida de sobreajuste ha salido:", sobreajuste)
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(datos, datos_out, w, w10, w2)
rm(aq, epsilon_n, error10, error2, escalado)
rm(i, j, N, Qf, sigma, sobreajuste, valor_real)
rm(valorg10, valorg2, y_n, f)
rm(Eout2, Eout10)

## ------------------------------------------------------------------------
#############
cat("EJERCICIO 3\n")
#############

## ------------------------------------------------------------------------
cat("APARTADO 1\n")

# Volvemos a fijar la semilla
set.seed(12345)

# Método de regresión lineal con weigth decay. Recibe los datos y las etiquetas
# por parámetros, junto con el weigth decay lambda.
Regress_Lin_WD <- function(datos, label, lambda) {
  Z <- t(datos)%*%datos
  dim <- nrow(Z)
  Z <- Z + diag(lambda, dim, dim)
  inversa <- solve(Z)
  wreg <- inversa %*% t(datos) %*% label
  return(wreg)
}

## ------------------------------------------------------------------------
d <- 3
sigma <- 0.5

wf <- unlist(simula_gauss(1, d+1, 1))
N <- 18
errores <- vector("list", 11)
i <- 1
while (N <= 118) {
  lambda <- 0.05/N
  datos <- simula_gauss(N, 3, sigma^2)
  datos <- matrix(unlist(datos), N, 3, T)
  datos <- cbind(datos,1)
  # Generamos los y_n
  y_n <- sapply(1:N, function(i) crossprod(datos[i,],wf)+sigma*rnorm(1))
  
  e <- vector("numeric", N)
  
  # Hacemos leave one out
  for(j in 1:N) {
    datos_loo <- datos[-j,]
    yn_loo <- y_n[-j]
    xn <- datos[j,]
    # Estimamos wreg con regresión lineal con weigth decay
    wreg <- Regress_Lin_WD(datos_loo, yn_loo, lambda)
    # Calculamos el error de validación cruzada
    valor_estimado <- crossprod(wreg,xn)
    e_n <- (y_n[j] - valor_estimado)^2
    e[j] <- e_n
  }
  errores[[i]] <- e
  
  N <- N+10
  i <- i+1
}

## ------------------------------------------------------------------------
listaEcv <- lapply(errores, function(i) mean(i))
cat("\nLa media de los errores de validación cruzada, Ecv, son los siguientes
    (para cada N)")
print(listaEcv)
parada()

## ------------------------------------------------------------------------
cat("\nNota: este apartado tarda un poco más de un minuto en ejecutarse\n")
wf <- unlist(simula_gauss(1, d+1, 1))
N <- 18
errores <- vector("list", 11)
varianzas <- vector("list", 11)
i <- 1
while (N <= 118) {
  e1 <- vector("numeric", 10^3)
  e2 <- vector("numeric", 10^3)
  Ecv <- vector("numeric", 10^3)
  for(k in 1:10^3) {
    lambda <- 0.05/N
    datos <- simula_gauss(N, 3, sigma^2)
    datos <- matrix(unlist(datos), N, 3, T)
    datos <- cbind(datos,1)
    # Generamos los y_n
    y_n <- sapply(1:N, function(i) crossprod(datos[i,],wf)+sigma*rnorm(1))
    
    e <- vector("numeric", N)
    
    # Hacemos leave one out
    for(j in 1:N) {
      datos_loo <- datos[-j,]
      yn_loo <- y_n[-j]
      xn <- datos[j,]
      # Estimamos wreg con regresión lineal con weigth decay
      wreg <- Regress_Lin_WD(datos_loo, yn_loo, lambda)
      # Calculamos el error de validación cruzada
      valor_estimado <- crossprod(wreg,xn)
      e_n <- (y_n[j] - valor_estimado)^2
      e[j] <- e_n
    }
    e1[k] <- e[1]
    e2[k] <- e[2]
    Ecv[k] <- mean(e)
  }
  
  errores[[i]] <- c(mean(e1), mean(e2), mean(Ecv))
  varianzas[[i]] <- c(var(e1), var(e2), var(Ecv))
  i <- i+1
  N <- N+10
}

## ------------------------------------------------------------------------
print("\nLa media de e1, e2 y Ecv ha sido, para cada N y en este orden:")
print(errores)
parada()
print("\nLa varianza de e1, e2 y Ecv ha sido, para cada N y este orden:")
print(varianzas)
parada()

## ------------------------------------------------------------------------
color <- rep(6:8,11)
N <- c(rep(18,3), rep(28,3), rep(38,3), rep(48, 3), rep(58,3), rep(68,3),
       rep(78,3), rep(88,3), rep(98,3), rep(108,3), rep(118,3))
pinta_particion(N, unlist(errores), color, xlab="Valor de N", 
                ylab="Valores medios de e1, e2 y Ecv", 
                main="Valores medios de e1, e2 y Ecv")
parada()

## ------------------------------------------------------------------------
pinta_particion(N, unlist(varianzas), color, xlab="Valor de N", 
                ylab="Varianzas de e1, e2 y Ecv", 
                main="Varianzas de e1, e2 y Ecv")
parada()

## ------------------------------------------------------------------------
e_1 <- rapply(varianzas, function(x) x[1])
Ecv <- rapply(varianzas, function(x) x[3])
N <- seq(18, 118, by=10)
Neff <- 100*(e_1/Ecv)/N
pinta_particion(N, Neff, rep(1,11), main="Neff", xlab="Valor de N",
                ylab="Neff como porcentaje de N")
parada()

## ------------------------------------------------------------------------
cat("\nNota:este apartado tarda un poco más de un minuto en ejecutarse\n")
wf <- unlist(simula_gauss(1, d+1, 1))
N <- 18
errores <- vector("list", 11)
varianzas <- vector("list", 11)
i <- 1
while (N <= 118) {
  e1 <- vector("numeric", 10^3)
  e2 <- vector("numeric", 10^3)
  Ecv <- vector("numeric", 10^3)
  for(k in 1:10^3) {
    lambda <- 2.5/N
    datos <- simula_gauss(N, 3, sigma^2)
    datos <- matrix(unlist(datos), N, 3, T)
    datos <- cbind(datos,1)
    # Generamos los y_n
    y_n <- sapply(1:N, function(i) crossprod(datos[i,],wf)+sigma*rnorm(1))
    
    e <- vector("numeric", N)
    
    # Hacemos leave one out
    for(j in 1:N) {
      datos_loo <- datos[-j,]
      yn_loo <- y_n[-j]
      xn <- datos[j,]
      # Estimamos wreg con regresión lineal con weigth decay
      wreg <- Regress_Lin_WD(datos_loo, yn_loo, lambda)
      # Calculamos el error de validación cruzada
      valor_estimado <- crossprod(wreg,xn)
      e_n <- (y_n[j] - valor_estimado)^2
      e[j] <- e_n
    }
    e1[k] <- e[1]
    e2[k] <- e[2]
    Ecv[k] <- mean(e)
  }
  
  errores[[i]] <- c(mean(e1), mean(e2), mean(Ecv))
  varianzas[[i]] <- c(var(e1), var(e2), var(Ecv))
  i <- i+1
  N <- N+10
}

## ------------------------------------------------------------------------
print("\nLa media de e1, e2 y Ecv ha sido, para cada N y en este orden:")
print(errores)
parada()
print("\nLa varianza de e1, e2 y Ecv ha sido, para cada N y este orden:")
print(varianzas)
parada()

## ------------------------------------------------------------------------
N <- c(rep(18,3), rep(28,3), rep(38,3), rep(48, 3), rep(58,3), rep(68,3),
       rep(78,3), rep(88,3), rep(98,3), rep(108,3), rep(118,3))
pinta_particion(N, unlist(errores), color, xlab="Valor de N", 
                ylab="Valores medios de e1, e2 y Ecv", 
                main="Valores medios de e1, e2 y Ecv")
parada()

## ------------------------------------------------------------------------
pinta_particion(N, unlist(varianzas), color, xlab="Valor de N", 
                ylab="Varianzas de e1, e2 y Ecv", 
                main="Varianzas de e1, e2 y Ecv")
parada()

## ------------------------------------------------------------------------
e_1 <- rapply(varianzas, function(x) x[1])
Ecv <- rapply(varianzas, function(x) x[3])
N <- seq(18, 118, by=10)
Neff <- 100*(e_1/Ecv)/N
pinta_particion(N, Neff, rep(1,11), main="Neff", xlab="Valor de N",
                ylab="Neff como porcentaje de N")
parada()

## ------------------------------------------------------------------------
# Borramos lo que no necesitamos
rm(datos, datos_loo, e_n, valor_estimado, wreg)
rm(color, d, e, e_1, e1, e2, Ecv, errores, i, j, k)
rm(lambda, listaEcv, N, Neff, sigma, varianzas, wf)
rm(xn, y_n, yn_loo)

