puntuacion <- function() {
	x <- sample.int(6,1)
	y <- sample.int(6,1)
	x + y
}


for (i in 1:10) print(puntuacion())

# Primera forma
x <- c()
for (i in 1:100) {
	x <- c(x, puntuacion())
}

# Segunda forma
hacer.vector <- function(n) {
	x <- c()
	for (i in 1:n) {
		x <- c(x, puntuacion())
	}
	x
}
hacer.vector(100)


#Baraja española
palos <- c(rep("Oros", times=12), rep("Copas", times=12), rep("Espadas", times=12), rep("Bastos", times=12))
numeros <- c(rep(1:12, times=4))
nombres <- c(rep(c(1:9, "Sota", "Caballo", "Rey"), times=4))
baraja <- data.frame(palos, numeros, nombres)
barajaRobadas <- data.frame(palos=character(), numeros=integer(), nombres=character())
NumCartasEnBaraja <- 48

#Función para barajar
#barajar <- function(baraja) {
#	permutacion <- sample(c(1:48), 48)
#	nueva_baraja <- baraja
#	for (i in 1:48) {
#		nueva_baraja[i,] <- baraja[permutacion[i],]
#	}
#	nueva_baraja
#}
barajar <- function(baraja) {
	assign(deparse(substitute(baraja)), baraja[sample(NumCartasEnBaraja),], 1)
}

#Función para robar una carta y meterla en el mazo de las robajas
robarCarta <- function(baraja) {
	carta <- baraja[1,]
	assign(deparse(substitute(NumCartasEnBaraja)), NumCartasEnBaraja-1, 1)
	assign(deparse(substitute(baraja)), baraja[-1,], 1)
	assign(deparse(substitute(barajaRobadas)), rbind(barajaRobadas, carta), 1)
	carta
}

