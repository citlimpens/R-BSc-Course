#Primero, nos acomodamos en el directorio en el que vamos a trabajar, que es en el que estar??n los datos que bajaremos de la red
setwd("/Users/Cit/Downloads")
#Comprobamos que s?? estamos en el directorio correcto
getwd()
#Guardamos el data set
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/zoo/zoo.data"
zoo <- xmlToDataFrame(url)
#Se crea la lista con los nombres que va a contener cada categor??a
nombres <- c("Animal", "Pelo(S/N)", "Plumas(S/N)", "Oviparo(S/N)", "Mam??fero(S/N)", "A??reo(S/N)", "Acu??tico(S/N)", "Predador(S/N)", "Dentado(S/N)", "Vertebrado(S/N)", "Respiraci??n(S/N)", "Venenoso(S/N)", "Aletas(S/N)", "Extremidades", "Cola(S/N)", "Domestico(S/N)", "Tama??o portatil(S/N)", "Tipo" )
#Se guarda como dataframe en una variable, este paso es equivalente al de la variable zoo, pero preferi intentar con ambos comandos
datoszoo <- read.csv("zoo.data", header = FALSE, as.is = T)
str(datoszoo)
#Se le ponen los nombras en la parte de arriba de las variables
#Igual que en el paso anterior, lo hice con las dos que guard??
colnames(datoszoo) <- nombres
colnames(zoo) <- nombres
#Para obtener un resumen num??rico y gr??fico
summary(zoo)
summary(datoszoo)

#Tambi??n se pueden dar resumenes de cada una de las categorias
summary(zoo$Tipo)
summary(zoo$Extremidades)
sd(zoo$Extremidades)
boxplot(zoo$Extremidades)
shapiro.test(zoo$Extremidades)

