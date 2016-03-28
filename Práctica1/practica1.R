## ------------------------------------------------------------------------
set.seed(237)
# Para que muestre las gráficas por pantalla
X11()

## ------------------------------------------------------------------------
cat("Ejercicio 1\n")
cat("Apartado 1\n")
simula_unif <- function(N, dim, rango) {
  lapply(1:N, function(x) runif(dim, min = rango[1], max = rango[2]))
}

## ------------------------------------------------------------------------
l <- simula_unif(4,3,c(0,1))
cat("Lista de longitud 4 y dimensión 3 en el rango (0,1)\n")
print(l)
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 2\n")
simula_gauss <- function(N, dim, sigma) {
  lapply(1:N, function(x) rnorm(dim, mean = 0, sqrt(sigma)))
}

## ------------------------------------------------------------------------
s <- simula_gauss(3,2,c(1,7))
cat("Lista de longitud 3 y dimensión 2 con varianza (1,7)\n")
print(s)
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 3\n")
lista <- simula_unif(50, 2, c(-50,50))
# Obtenemos las coordenadas x e y, que son la primera y segunda columna
# de la lista (primera componente y segunda componente de cada vector en la lista)
x <- rapply(lista, function(x) x[1])
y <- rapply(lista, function(x) x[2])
plot(x, y, type = "p", xlab = "Eje X", ylab = "Eje Y", main="Datos uniformes")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Apartado 4\n")
lista2 <- simula_gauss(50, 2, c(5,7))
# Obtenemos las coordenadas x e y, que son la primera y segunda columna
# de la lista (primera componente y segunda componente de cada vector en la lista)
x <- rapply(lista2, function(x) x[1])
y <- rapply(lista2, function(x) x[2])
plot(x, y, type = "p", xlab = "Eje X", ylab = "Eje Y", main="Datos gaussianos")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 5\n")
simula_recta <- function(intervalo) {
  l <- simula_unif(2, 2, intervalo)
  a <- (l[[2]][2] - l[[1]][2]) / (l[[2]][1] - l[[1]][1])
  b <- l[[1]][2] - a * l[[1]][1]
  c(a,b)
}

## ------------------------------------------------------------------------
cat("Coeficientes de la recta que corta al cuadrado (-50,50)x(-50,50)\n")
print(simula_recta(c(-50,50)))
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 6\n")
etiquetar <- function(valor) {
  if (valor > 0) {
    return(+1)
  }
  else {
    return(-1)
  }
}

## ------------------------------------------------------------------------
r <- simula_recta(c(-50,50))
etiquetas <- lapply(1:length(lista), function(i) {
  # Obtenemos los puntos uno a uno y los etiquetamos
  p <- lista[[i]]
  f <- p[2] - r[1]*p[1] - r[2]
  etiquetar(f)
})

## ------------------------------------------------------------------------
etiquetas <- unlist(etiquetas)

## ------------------------------------------------------------------------
pinta_particion <- function(lista_puntos, etiquetas=NULL, visible=FALSE, f=NULL,
                            rango = c(-50,50), main="") {
  x <- rapply(lista_puntos, function(x) x[1])
  y <- rapply(lista_puntos, function(x) x[2])
  if(is.null(etiquetas))
     etiquetas=1
  else etiquetas = etiquetas+3
  plot(x, y, type = "p", col = etiquetas, xlab = "Eje X", ylab = "Eje Y", 
       xlim = rango, ylim = rango, main = main)
  
  # Si queremos pintar función junto con los datos
  if(visible) {
    sec <- seq(-60, 60, length.out = 1500)
    z <- outer(sec, sec, f)
    contour(sec, sec, z, col = "violet", levels = 0, add = TRUE, drawlabels = F)
  }
}

## ------------------------------------------------------------------------
pinta_particion(lista, etiquetas, TRUE, function(x,y) r[1]*x-y+r[2], 
                main="Puntos etiquetados con recta")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 7\n")
etiquetasFA <- lapply(1:length(lista), function(i) {
  # Obtenemos los puntos uno a uno y los etiquetamos
  p <- lista[[i]]
  f1 <- (p[1]-10)^2 + (p[2] - 20)^2 - 400
  etiquetar(f1)
})
etiquetasFA <- unlist(etiquetasFA)

pinta_particion(lista, etiquetasFA, TRUE, function(x,y) (x-10)^2 + (y-20)^2 -
                  400, main="Puntos etiquetados con función a")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------

etiquetasFB <- lapply(1:length(lista), function(i) {
  # Obtenemos los puntos uno a uno y los etiquetamos
  p <- lista[[i]]
  f2 <- 0.5*(p[1]+10)^2 + (p[2] - 20)^2 - 400
  etiquetar(f2)
})
etiquetasFB <- unlist(etiquetasFB)

pinta_particion(lista, etiquetasFB, TRUE, function(x,y) 0.5*(x+10)^2 + (y-20)^2
                - 400, main="Puntos etiquetados con función b")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------

etiquetasFC <- lapply(1:length(lista), function(i) {
  # Obtenemos los puntos uno a uno y los etiquetamos
  p <- lista[[i]]
  f3 <- 0.5*(p[1]-10)^2 - (p[2] + 20)^2 - 400
  etiquetar(f3)
})
etiquetasFC <- unlist(etiquetasFC)

pinta_particion(lista, etiquetasFC, TRUE, function(x,y) 0.5*(x-10)^2 - (y+20)^2
                - 400, main="Puntos etiquetados con función c")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------

etiquetasFD <- lapply(1:length(lista), function(i) {
  # Obtenemos los puntos uno a uno y los etiquetamos
  p <- lista[[i]]
  f4 <- p[2] - 20*(p[1]^2) - 5*p[1] + 3
  etiquetar(f4)
})
etiquetasFD <- unlist(etiquetasFD)
pinta_particion(lista, etiquetasFD, TRUE, function(x,y) y - 20*x^2 - 5*x + 3, 
                main="Puntos etiquetados con función d")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Apartado 8\n")
cambiar_etiquetas <- function(etiquetas) {
  num <- 1:length(etiquetas)
  etiquetas_cambiadas <- etiquetas
  # Cogemos las posiciones de las etiquetas positivas y negativas
  positivos <- num[etiquetas > 0]
  negativos <- num[etiquetas < 0]
  # Comprobamos que hay algún elemento que cambiar y obtenemos el 10%
  # de posiciones aleatorias
  if(length(positivos)*0.1 > 0) {
    cambiar1 <- sample(positivos, length(positivos)*0.1)
    # Cambiamos las etiquetas que hemos obtenido antes
    etiquetas_cambiadas[cambiar1] <- -1
  }
  if(length(negativos)*0.1 > 0) {
    cambiar2 <- sample(negativos, length(negativos)*0.1)
    # Cambiamos las etiquetas que hemos obtenido antes
    etiquetas_cambiadas[cambiar2] <- +1
  }
  # Devolvemos las etiquetas cambiadas
  etiquetas_cambiadas
}

# Llamamos a esta función para cambiar las etiquetas y pintamos los datos con
# estas nuevas etiquetas
etiquetas2 <- cambiar_etiquetas(etiquetas)
pinta_particion(lista, etiquetas2, TRUE, function(x,y) r[1]*x-y+r[2], 
                main="Puntos etiquetados con recta y ruido")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
pinta_particion(lista, cambiar_etiquetas(etiquetasFA), TRUE, 
                function(x,y) (x-10)^2 + (y-20)^2 - 400, 
                main="Puntos etiquetados con función a y ruido")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
pinta_particion(lista, cambiar_etiquetas(etiquetasFB), TRUE, 
                function(x,y) 0.5*(x+10)^2 + (y-20)^2 - 400, 
                main="Puntos etiquetados con función b y ruido")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
pinta_particion(lista, cambiar_etiquetas(etiquetasFC), TRUE, 
                function(x,y) 0.5*(x-10)^2 - (y+20)^2 - 400, 
                main="Puntos etiquetados con función c y ruido")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
pinta_particion(lista, cambiar_etiquetas(etiquetasFD), TRUE, 
                function(x,y) y - 20*x^2 - 5*x + 3, 
                main="Puntos etiquetados con función d y ruido")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Ejercicio 2\n")
cat("Apartado 1\n")
ajusta_PLA <- function(datos, label, max_iter, vini) {
  parada <- F
  fin <- F
  w <- vini
  iter <- 1
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
  # Devolvemos el hiperplano y el número máximo de iteraciones al que hemos
  # llegado.
  list(w, iter)
}


## ------------------------------------------------------------------------
cat("Apartado 2\n")
# Metemos los datos, que teníamos en una lista llamada "lista" en
# una matriz.
m <- matrix(unlist(lista), 50, 2, byrow=TRUE)
datos <- matrix(1, 50, 3)
datos[1:50, 1:2] <- m
sol <- ajusta_PLA(datos, etiquetas, 20, c(0,0,0))
iter1 <- sol[[2]]
cat("El número de iteraciones necesarias para converger en PLA ha sido:\n")
print(iter1)
w <- sol[[1]]
w <- -w / w[2]
pinta_particion(lista, etiquetas, TRUE, function(x,y) y-w[3]-w[1]*x, 
                main="Solución algoritmo PLA")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
# Metemos los 10 vectores iniciales aleatorios en una lista
waleatorios <- simula_unif(10, 3, c(0,1))
iteraciones <- lapply(1:10, function(i) {
  wi <- waleatorios[[i]]
  sol <- ajusta_PLA(datos, etiquetas, 200, wi)
  sol[[2]]
})

iteraciones <- unlist(iteraciones)
cat("La media de las iteraciones para que el PLA converja ha sido: \n")
print(mean(iteraciones))
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Apartado 3\n")
cuenta_diferencias <- function(etiquetas1, etiquetas2) {
  vf <- etiquetas1 == etiquetas2
  length(vf[vf == FALSE])
}

## ------------------------------------------------------------------------
cuenta_errores <- function(sol_PLA, etiquetas_originales) {
  w <- sol_PLA[[1]]
  w <- -w / w[2]
  # Recordemos que los datos que hay en la matriz "datos" son los mismos
  # puntos que hay en la matriz "lista"
  etiquetas_cambiadas <- lapply(1:length(lista), function(i) {
    # Obtenemos los puntos uno a uno y los etiquetamos
    p <- lista[[i]]
    f <- -w[1]*p[1] + p[2] - w[3]
    etiquetar(f)
  })
  # Devolvemos el número de errores que da la solución
  cuenta_diferencias(etiquetas_originales, etiquetas_cambiadas)
}

## ------------------------------------------------------------------------
sol1 <- ajusta_PLA(datos, etiquetas2, 10, c(0,0,0))
cat("El número de fallos para 10 iteraciones ha sido:\n")
print(cuenta_errores(sol1, etiquetas2))
w1 <- sol1[[1]] 
w1 <- -w1/w1[2]
pinta_particion(lista, etiquetas2, TRUE, function(x,y) w1[1]*x-y+w1[3],
                main="Solución PLA para 10 iteraciones")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol2 <- ajusta_PLA(datos, etiquetas2, 100, c(0,0,0))
cat("El número de fallos para 100 iteraciones ha sido:\n")
print(cuenta_errores(sol2, etiquetas2))
w2 <- sol2[[1]] 
w2 <- -w2/w2[2]
pinta_particion(lista, etiquetas2, TRUE, function(x,y) w2[1]*x-y+w2[3], 
                main="Solución PLA para 100 iteraciones")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol3 <- ajusta_PLA(datos, etiquetas2, 1000, c(0,0,0))
cat("El número de fallos para 1000 iteraciones ha sido:\n")
print(cuenta_errores(sol3, etiquetas2))
w3 <- sol3[[1]] 
w3 <- -w3/w3[2]
pinta_particion(lista, etiquetas2, TRUE, function(x,y) w3[1]*x-y+w3[3], 
                main = "Solución PLA para 1000 iteraciones")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 4\n")
sol1 <- ajusta_PLA(datos, etiquetasFA, 10, c(0,0,0))
cat("El número de fallos con la primera función del apartado 7 y 10 iteraciones ha sido:\n")
print(cuenta_errores(sol1, etiquetasFA))
w1 <- sol1[[1]] 
w1 <- -w1 / w1[2]
pinta_particion(lista, etiquetasFA, TRUE, function(x,y) w1[1]*x-y+w1[3], 
                main="Solución PLA para 10 iteraciones")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol2 <- ajusta_PLA(datos, etiquetasFA, 100, c(0,0,0))
cat("El número de fallos con la primera función del apartado 7 y 100 iteraciones ha sido:\n")
print(cuenta_errores(sol2, etiquetasFA))
w2 <- sol2[[1]] 
w2 <- -w2 / w2[2]
pinta_particion(lista, etiquetasFA, TRUE, function(x,y) w2[1]*x-y+w2[3], 
                main="Solución PLA para 100 iteraciones")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol3 <- ajusta_PLA(datos, etiquetasFA, 1000, c(0,0,0))
cat("El número de fallos con la primera función del apartado 7 y 1000 iteraciones ha sido:\n")
print(cuenta_errores(sol3, etiquetasFA))
w3 <- sol3[[1]] 
w3 <- -w3 / w3[2]
pinta_particion(lista, etiquetasFA, TRUE, function(x,y) w3[1]*x-y+w3[3], 
                main="Solución PLA para 1000 iteraciones")
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 5\n")
ajusta_PLA_sol <- function(datos, label, max_iter, vini) {
  parada <- F
  fin <- F
  w <- vini
  iter <- 1
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
    
    # Pintamos la gráfica
    w2 <- -w / w[2]
    pinta_particion(lista, label, TRUE, function(x,y) -w2[1]*x + y - w2[3])
  
    # Paramos la ejecución para que se pueda ver la gráfica durante medio segundo
    Sys.sleep(1)
    
    # Si no se ha entrado en el if, todos los datos estaban bien
    # clasificados y podemos poner a TRUE la variable parada.
    if(fin == T) {
      parada = T
    }
    else {
      fin = T
    }
    iter <- iter + 1
    if (iter >= max_iter) {parada = T}
  }
  # Devolvemos el hiperplano y el número máximo de iteraciones al que hemos
  # llegado.
  list(w, iter)
}

## ------------------------------------------------------------------------
# Llamamos a la función
ajusta_PLA_sol(datos, etiquetas2, 10, c(0,0,0))
cat("Presione una tecla para continuar")
t <- readline()
ajusta_PLA_sol(datos, etiquetas2, 100, c(0,0,0))
cat("Presione una tecla para continuar")
t <- readline()
cat("Nota: la ejecución con 1000 iteraciones está comentada. 
    Descomentar si se quiere ver.")
#ajusta_PLA_sol(datos, etiquetas2, 1000, c(0,0,0))
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 6\n")
ajusta_PLA_MOD <- function(datos, label, max_iter, vini) {
  parada <- F
  fin <- F
  w <- vini
  wmejor <- w
  iter <- 1
  errores_mejor <- nrow(datos)
  
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
    l <- list(w, 1)
    errores_actual <- cuenta_errores(l, label)
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
  list(wmejor, iter, errores_mejor)
}

## ------------------------------------------------------------------------
sol1_MOD <- ajusta_PLA_MOD(datos, etiquetasFA, 1000, c(0,0,0))
w1 <- sol1_MOD[[1]]
errores <- sol1_MOD[[3]]
w1 <- -w1 / w1[2]
pinta_particion(lista, etiquetasFA, TRUE, function(x,y) -w1[1]*x + y - w1[3],
                 main="Solución PLA MOD para función a")
cat("El número de errores para el PLA MOD y función a ha sido:\n")
print(errores)
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol2_MOD <- ajusta_PLA_MOD(datos, etiquetasFB, 1000, c(0,0,0))
w2 <- sol2_MOD[[1]]
errores <- sol2_MOD[[3]]
w2 <- -w2 / w2[2]
pinta_particion(lista, etiquetasFB, TRUE, function(x,y) -w2[1]*x + y - w2[3], 
                main="Solución PLA MOD para función b")
cat("El número de errores para el PLA MOD y función b ha sido:\n")
print(errores)
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol3_MOD <- ajusta_PLA_MOD(datos, etiquetasFC, 1000, c(0,0,0))
w3 <- sol3_MOD[[1]]
errores <- sol3_MOD[[3]]
w3 <- -w3 / w3[2]
pinta_particion(lista, etiquetasFC, TRUE, function(x,y) -w3[1]*x + y - w3[3], 
                main="Solución PLA MOD para función c")
cat("El número de errores para el PLA MOD y función c ha sido:\n")
print(errores)
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
sol4_MOD <- ajusta_PLA_MOD(datos, etiquetasFD, 1000, c(0,0,0))
w4 <- sol4_MOD[[1]]
errores <- sol4_MOD[[3]]
w4 <- -w4 / w4[2]
pinta_particion(lista, etiquetasFD, TRUE, function(x,y) -w3[1]*x + y - w3[3], 
                main="Solución PLA MOD para función d")
cat("El número de errores para el PLA MOD y función d ha sido:\n")
print(errores)
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Ejercicio 3\n")
cat("Apartado 2\n")
data <- read.table("datos/zip.train", sep=" ")
numero <- data$V1
frame_num1 <- data[numero==1,]
frame_num5 <- data[numero==5,]
# Eliminamos de cada uno la primera columna, que guarda el número del que
# son los datos, y la última, que tiene NA
frame_num5 <- frame_num5[,-258]
frame_num1 <- frame_num1[,-258]
frame_num5 <- frame_num5[,-1]
frame_num1 <- frame_num1[,-1]

# Las dos siguientes líneas pasan estos dos data.frame a matrices 
# y los siguen dejando por filas (para poder utilizar apply sobre una matriz)
datos_num1 <- data.matrix(frame_num1)
datos_num5 <- data.matrix(frame_num5)

# Lo siguiente hace una lista de matrices, una por cada instancia de número 1 o 5
lista_num5 <- lapply(split(datos_num5, seq(nrow(datos_num5))), function(x) {
    matrix(x, 16, 16, T)
})
lista_num1 <- lapply(split(datos_num1, seq(nrow(datos_num1))), function(x) {
    matrix(x, 16, 16, T)
})

# Mostramos una imagen de un 1 y una imagen de un 5
image(lista_num1[[1]])
cat("Presione una tecla para continuar")
t <- readline()
image(lista_num5[[1]])
cat("Presione una tecla para continuar")
t <- readline()

## ------------------------------------------------------------------------
cat("Apartado 3\n")
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
cat("Apartado 4\n")
pinta_grafica <- function(coordX, coordY, etiquetas=NULL, visible=FALSE, a=NULL, b=NULL, 
                          xlab="Intensidad Promedio", ylab="Simetría", main="") {
  if(is.null(etiquetas))
     etiquetas=1
  else etiquetas = etiquetas+3
  
  plot(coordX, coordY, type = "p", col = etiquetas, xlab = xlab, ylab = ylab, 
       main = main)
  
  if(visible) {
    abline(b,a)
  }
}


## ------------------------------------------------------------------------

# Calculamos primero las intensidades y las simetrías de todas las matrices, 
# es decir, de todas las instancias de 1's y 5's
simetria_1 <- lapply(lista_num1, function(m) calcular_simetria(m))
simetria_5 <- lapply(lista_num5, function(m) calcular_simetria(m))
intensidad_1 <- lapply(lista_num1, function(m) mean(m))
intensidad_5 <- lapply(lista_num5, function(m) mean(m))

# Ponemos las listas anteriores como vectores
simetria_1 <- unlist(simetria_1)
simetria_5 <- unlist(simetria_5)
intensidad_1 <- unlist(intensidad_1)
intensidad_5 <- unlist(intensidad_5)

# Pintamos en una gráfica la intensidad y simetría de las instancias de ambos
# números
intensidad <- c(intensidad_1, intensidad_5)
simetria <- c(simetria_1, simetria_5)
color <- c(rep.int(0, length(intensidad_1)), rep.int(2, length(intensidad_5)))
pinta_grafica(intensidad, simetria, color)
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Apartado 5\n")
Regress_Lin <- function(datos, label) {
  descomp <- La.svd(datos)
  vt <- descomp[[3]]
  # Creamos la inversa de la matriz diagonal al cuadrado
  diag <- matrix(0, length(descomp[[1]]), length(descomp[[1]]))
  for (i in 1:length(descomp[[1]])) {
    diag[i,i] = descomp[[1]][i]
    if (diag[i,i] != 0) {
      diag[i,i] = 1/(diag[i,i]^2)
    }
  }
  prod_inv <- t(vt) %*% diag %*% vt
  pseud_inv <- prod_inv %*% t(datos)
  
  w <- pseud_inv %*% label
  w
}


## ------------------------------------------------------------------------
cat("Apartado 6\n")
w <- Regress_Lin(cbind(intensidad, 1), simetria)
pinta_grafica(intensidad, simetria, color, TRUE, w[1], w[2], 
              main="Regresión")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Apartado 7\n")
N <- 100
datos <- simula_unif(N, 2, c(-10,10))
#Pasamos la lista de datos una matriz con la tercera columna a 1
datos <- unlist(datos)
datos <- matrix(datos, N, 2, T)
datos <- cbind(datos, 1)

generaEtiquetas <- function() {
  r <- simula_recta(c(-10,10))
  etiquetas <- lapply(1:nrow(datos), function(i) {
    #Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos[i,]
    f <- p[2] - r[1]*p[1] - r[2]
    etiquetar(f)
  })
  
  etiquetas <- unlist(etiquetas)
  etiquetas
}


## ------------------------------------------------------------------------
errores <- 0
for (i in 1:1000) {
  etiquetas <- generaEtiquetas()
  # Obtenemos g por regresión lineal
  w <- Regress_Lin(datos, etiquetas)
  # Y calculamos las etiquetas que da esta g
  etiquetas_cambiadas <- unlist(lapply(1:nrow(datos), function(i) {
    p <- datos[i, ]
    sign(crossprod(w,p))
  }))
  # Contamos las diferencias entre los dos vectores de etiquetas
  errores <- errores + cuenta_diferencias(etiquetas, etiquetas_cambiadas)
}

# Hacemos la media de los errores, teniendo en cuenta que hemos hecho 1000
# iteraciones
errores <- errores / 1000

w <- -w / w[2]
# Pintamos la última de las rectas, por ver cómo lo está haciendo
pinta_grafica(datos[,1], datos[,2], etiquetas, TRUE, w[1], w[3], "Eje X", 
              "Eje Y", main="Regresión lineal para clasificación")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
Ein <- 100*errores/N
cat("El porcentaje de puntos mal clasificados dentro de la muestra, Ein:\n")
print(Ein)
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
N <- 100
datos <- simula_unif(N, 2, c(-10,10))
# Pasamos la lista de datos una matriz con la tercera columna a 1
datos <- unlist(datos)
datos <- matrix(datos, N, 2, T)
datos <- cbind(datos, 1)

r <- simula_recta(c(-10,10))
etiquetas_f <- unlist(lapply(1:nrow(datos), function(i) {
  # Obtenemos los puntos uno a uno y los etiquetamos
  p <- datos[i,]
  f <- p[2] - r[1]*p[1] - r[2]
  etiquetar(f)
}))

# Obtenemos g por regresión
g <- Regress_Lin(datos, etiquetas_f)

# Contamos los errores
errores <- 0
for (i in 1:1000) {
  # Generamos los nuevos datos (los de fuera de la muestra) y los pasamos a 
  # una matriz
  datos_out <- simula_unif(1000, 2, c(-10,10))
  datos_out <- unlist(datos_out)
  datos_out <- matrix(datos_out, 100, 2, T)
  datos_out <- cbind(datos_out, 1)

  etiquetas_originales <- unlist(lapply(1:nrow(datos_out), function(i) {
    # Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos_out[i,]
    f <- p[2] - r[1]*p[1] - r[2]
    etiquetar(f)
  }))
  etiquetas_g <- unlist(lapply(1:nrow(datos_out), function(i) {
    # Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos_out[i,]
    sign(crossprod(g,p))
  }))
  
  errores <- errores + cuenta_diferencias(etiquetas_originales, etiquetas_g)
}
# Hacemos la media de los errores
errores <- errores / 1000

Eout = 100*errores/N
cat("El porcentaje de puntos mal clasificados fuera de la muestra, Eout:\n")
print(Eout)
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
N <- 10
datos <- simula_unif(N, 2, c(-10,10))
#Pasamos la lista de datos una matriz con la tercera columna a 1
datos <- unlist(datos)
datos <- matrix(datos, N, 2, T)
datos <- cbind(datos, 1)

iteraciones <- 0

for (i in 1:1000) {
  #Obtenemos una recta aleatoria, etiquetamos los datos en base a esa recta y 
  #obtenemos g, un vector de pesos inicial para el PLA
  r <- simula_recta(c(-10,10))
  etiquetas_f <- unlist(lapply(1:nrow(datos), function(i) {
    #Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos[i,]
    f <- p[2] - r[1]*p[1] - r[2]
    etiquetar(f)
  }))
  
  g <- Regress_Lin(datos, etiquetas_f)
  
  #Le pasamos al PLA g como vector inicial y nos quedamos con el número de 
  #iteraciones que tarda en converger
  sol <- ajusta_PLA(datos, etiquetas_f, 1000, g)
  iteraciones <- iteraciones + sol[[2]]
}

iteraciones <- iteraciones / 1000
cat("Número medio de iteraciones para PLA converja:\n")
print(iteraciones)
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
cat("Apartado 8\n")
errores <- 0
for (i in 1:1000) {
  N <- 1000
  #Generamos la muestra de puntos
  datos <- simula_unif(N, 2, c(-10,10))
  #Pasamos la lista de datos una matriz con la tercera columna a 1
  datos <- unlist(datos)
  datos <- matrix(datos, N, 2, T)
  datos <- cbind(datos, 1)
  
  #Etiquetamos los puntos en base a la f dada
  etiquetas_f <- unlist(lapply(1:nrow(datos), function(i) {
    #Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos[i,]
    f <- p[1]^2 + p[2]^2 - 25
    etiquetar(f)
  }))
  
  #Generamos ruido sobre estas etiquetas cambiando el signo a un 10% de estas
  #etiquetas con la función que ya tenemos
  etiquetas_f <- cambiar_etiquetas(etiquetas_f)
  
  #Generamos w por regresión
  w <- Regress_Lin(datos, etiquetas_f)
  etiquetas_cambiadas <- unlist(lapply(1:nrow(datos), function(i) {
    p <- datos[i, ]
    sign(crossprod(w,p))
  }))
  errores <- errores + cuenta_diferencias(etiquetas_f, etiquetas_cambiadas)
}
errores <- errores/1000
E_in = 100*errores/N
cat("Porcentaje de números mal clasificados dentro de la muestra, Ein\n")
print(E_in)
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
N <- 1000
#Generamos la muestra de puntos
lista_puntos <- simula_unif(N, 2, c(-10,10))
x1 <- rapply(lista_puntos, function(x) x[1])
x2 <- rapply(lista_puntos, function(x) x[2])
datos <- cbind(1, x1, x2, x1*x2, x1^2, x2^2)

#Etiquetamos los puntos en base a la f dada
coord <- cbind(x1, x2)
etiquetas_f <- unlist(lapply(1:nrow(coord), function(i) {
  #Obtenemos los puntos uno a uno y los etiquetamos
  p <- coord[i,]
  f <- p[1]^2 + p[2]^2 - 25
  etiquetar(f)
}))

#Generamos ruido sobre las etiquetas
etiquetas_f = cambiar_etiquetas(etiquetas_f)

#Hacemos regresión
w <- Regress_Lin(datos, etiquetas_f)
w

pinta_particion(lista_puntos, etiquetas_f, TRUE, function(x,y) w[1] + w[2]*x + 
                  w[3]*y + w[4]*x*y + w[5]*x^2 + w[6]*y^2, c(-10,10), 
                main = "Regresión para clasificación con ruido en curva")
cat("Presione una tecla para continuar")
t <- readline()


## ------------------------------------------------------------------------
N <- 1000
errores <- 0
for (i in 1:1000) {
  #Generamos los nuevos 1000 puntos con los que calcular el error fuera
  lista_puntos <- simula_unif(N, 2, c(-10,10))
  x1 <- rapply(lista_puntos, function(x) x[1])
  x2 <- rapply(lista_puntos, function(x) x[2])
  datos_out <- cbind(1, x1, x2, x1*x2, x1^2, x2^2)
  
  etiquetas_originales <- unlist(lapply(1:nrow(datos_out), function(i) {
    #Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos_out[i,]
    f <- p[1]^2 + p[2]^2 - 25
    etiquetar(f)
  }))
  
  #Generamos ruido sobre las etiquetas originales
  etiquetas_originales <- cambiar_etiquetas(etiquetas_originales)
  
  etiquetas_w <- unlist(lapply(1:nrow(datos_out), function(i) {
    #Obtenemos los puntos uno a uno y los etiquetamos
    p <- datos_out[i,]
    sign(crossprod(w,p))
  }))
  
  #Contamos los errores
  errores <- errores + cuenta_diferencias(etiquetas_originales, etiquetas_w)

}

errores = errores/1000
Eout = 100*errores/N
cat("Porcentaje de puntos mal clasificado fuera de la muestra, Eout:\n")
print(Eout)
cat("Presione una tecla para continuar")
t <- readline()


