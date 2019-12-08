library(MPV)
datos0<-data.frame(x =c(1,1.5,2,3,4,4.5,5,5.5,6,6.5,7,8,9,10,11,12,13,14,15), 
                   y=c(6.3,11.1,20,24,26.1,30,33.8,34,38.1,39.9,42,46.1,53.1,
                       52,52.5,48,42,27.8,21.9))
mfrow=c(1,1)
plot(datos0$x, datos0$y)

data()
data("p7.2")
datos <- p7.2
str(datos)
plot(datos)
modelo0b <- lm(datos$y ~ datos$x)
class(modelo0b)
str(modelo0b)
modelo0b$coefficients
modelo0b$residuals
summary(modelo0b)
plot(modelo0b, which = 1)
shapiro.test(modelo0b$residuals)
ncvTest(modelo0b)
AICc(modelo0b)

#ajustar un modelo cuadratoco, se le hace a cada ajuste todas las mismas pruebas de arriba
x1 <- datos$x - mean(datos$x)
modelo0b1 <- lm(datos$y ~ x1)
x2 <- x1^2
modelo0b2 <- lm(datos$y ~ x1+x2)

#Para lo  que no se pueda resolver con ese tipo de datos, se tiene que cambiar por un tipo diferente
#bugs = modelos bayesianos
#Se pueden meter todos los modelos hechosw en una sola lista
lista_mods <- list()
lista_mods[[1]] <- modelo0b
losta_mods[[2]] <- modelo0b1
losta_mods[[3]] <- modelo0b2

