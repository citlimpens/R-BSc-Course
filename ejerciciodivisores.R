m <- readline("Escribe tu n??mero m ")
m <- as.integer(m)

lista <- c()
i = 1
while(i<= m){
  a <- i
  lista <- c(lista, a)
  i = i+1
}
lista 
divisores <- c()

for(i in lista){
  if(i%%m == 0){
    a = i
    divisores <- c(divisores, a)
    
  }
  
}
divisores