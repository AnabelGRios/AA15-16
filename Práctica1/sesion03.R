m[1,1] = -1
m[2,] = 1

d <- array(c(1:5, 1:5), c(5,2))
a[d] = -2
a

#Hacer diagrama de quesos
counts <- hist(x, breaks=10, plot=F)$counts
labels <- hist(x, breaks=10, plot=F)$breaks
pie(counts, labels=labels, main="hist")
