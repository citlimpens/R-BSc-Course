#Montgomery introduccion a la regresi??n
#y: cantidad de fracturas
#x1: espesor de la caega interior
#x2: Porcentaje de extracci??n del manto
#x3: altura del manto inferior
#x4: timepo
library(car)
library(MASS)
library(MPV)
data(p13.7)
datos <- p13.7
str(datos)
poissdatos <- glm(y ~ x1+x2+x3+x4, data = datos, family=poisson())
#step da el akaike m??s chiquito

residuales <- resid(poissdatos)
residuales
plot(residuales)

aicajust <- step(poissdatos)
residualesa <- resid(aicajust)
summary(aicajust)
residualesa
plot(residualesa)
hist(residualesa)
hist(residuales)

hist(datos$x1, xlab = "espesor de la carga")
hist(datos$x2, xlab = "porcentaje de la extracci??n")
hist(datos$x3, xlab = "altura del manto inferior")
hist(datos$x4, xlab = "tiempo")

#Haciedo modificaciones a los datos

#1. logaritmo
datoslog2 <- log2(datos)
summary(datoslog2)

hist(datoslog2$x1, xlab = "espesor de la carga")
hist(datoslog2$x2, xlab = "porcentaje de la extracci??n")
hist(datoslog2$x3, xlab = "altura del manto inferior")
hist(datoslog2$x4, xlab = "tiempo")

datoslog10 <- log10(datos)
summary(datoslog10)

hist(datoslog10$x1, xlab = "espesor de la carga")
hist(datoslog10$x2, xlab = "porcentaje de la extracci??n")
hist(datoslog10$x3, xlab = "altura del manto inferior")
hist(datoslog10$x4, xlab = "tiempo")

#Con los logaritmos base 2 y base 10 no se acomodan los datos, ahora se probar?? con exponentes
#El primero que se va a probar, es usando e elevado a nuestro valor

datosexp <- exp(datos)
summary(datosexp)

hist(datosexp$x1, xlab = "espesor de la carga")
hist(datosexp$x2, xlab = "porcentaje de la extracci??n")
hist(datosexp$x3, xlab = "altura del manto inferior")
hist(datosexp$x4, xlab = "tiempo")

#Esta modificaci??n no funcion?? de nada para la distribuci??n de los datos, al contrario
#Solamente se logr?? que se acumularan los datos en un solo sitio.

datos2 <- datos^2
datosfr <- data.frame(datos2)
poissdatos2 <- glm(y ~ x1+x2+x3+x4, data = datosfr, family=poisson())
poissdatos2 

hist(datosfr$x1, xlab = "espesor de la carga")
hist(datosfr$x2, xlab = "porcentaje de la extracci??n")
hist(datosfr$x3, xlab = "altura del manto inferior")
hist(datosfr$x4, xlab = "tiempo")

residuales2 <- resid(poissdatos2)
residuales2
plot(residuales2)

aicajust2 <- step(poissdatos2)
residualesb <- resid(aicajust2)
summary(aicajust2)
residualesb
plot(residualesb)

#Comparaci??n de los histogramas con la normal, la normal con step, y este ajuste con step
hist(residuales2)
hist(residualesb)
hist(residuales)
hist(residualesa)

#Se ve igual a como estaba anteriormente, muchos ajustes usan raiz cuadrada y le suman uno al resultado para no tener 0, por lo que ese es el siguiente paso

raizdatos <- (datos^(1/2))+1
datosfr2 <- data.frame(raizdatos)
poissdatos3 <- glm(y ~ x1+x2+x3+x4, data = datosfr2, family=poisson())
poissdatos3 

hist(datosfr2$x1, xlab = "espesor de la carga")
hist(datosfr2$x2, xlab = "porcentaje de la extracci??n")
hist(datosfr2$x3, xlab = "altura del manto inferior")
hist(datosfr2$x4, xlab = "tiempo")

residuales3 <- resid(poissdatos3)
residuales3
plot(residuales3)

aicajust3 <- step(poissdatos3)
residualesc <- resid(aicajust3)
summary(aicajust3)
residualesc
plot(residualesc)

#Comparaci??n de los histogramas con la normal, la normal con step, y este ajuste con step, junto con el ajuste y el normal de los datos elevados al cuadrado

hist(residuales3)
hist(residualesc)
hist(residuales2)
hist(residualesb)
hist(residuales)
hist(residualesa)

qqnorm(raizdatos)

#Estos datos no se pudieron ajustar de esta manera.
#La mejor opci??n, si se quiere una distribuci??n normal, ser??a la ra??z cuadrada, pero se deber??a seguir haciendo modificaciones apra encontrar una distribuci??n mejor
#Si se quiere una distribuci??n tipo de orden 0, los mismos datos normales, o elevados al cuadrado funcionan.
#A??n as??, como cada variable tiene cosas diferentes, y se comporta de manera diferente, lo m??s adecuado creo que ser??a ajustar directamente las variables que se quieran utilizar, retirando a las que no se requeiran. 
#Tomando en cuenta la devianza, adem??s, esta es la modificaci??n que tiene una menor, siendo de 13, mientras que las anteriores eran de 200-400