#El paquete a utilizar, se llama RGP, de R Genetic Programming

#Introducci??n: exploraci??n del paquete RGP 
#Definicion fenetic programming
install.packages("rgp")
library("rgp")
#Este paquete sirve para modelaje de din??micas evolutivas
#Primero de definen los s??mbolos, variables y constantes a utilizar en los modelos
funciones <- functionSet("+","*","-")
variables <- inputVariableSet("x")
constantes <- constantFactorySet(function() rnorm(1))

#Se tiene que definir algo llamado "fitness function", lo que define el problema a resolver mediante la GP (genetic programming)
#Esta fitness function le da un valor n??merico de fitness a lo que se va a modelar
#Primero, se define un intervalo en el que se va a trabajar, el que se usa por default es entre -pi y pi, pero se puede usar cualquiera. Para verificar su funcionalidad con otros intervalos, utilizar?? e
#El by indica de cuanto en cuanto se van a saltar, para tener la secuencia de valores completa
#En el intervalo es donde se puede poner la lista de valores que se quieran modelar

e <- 2.718281
intervalo <- seq(from = -e, to = e, by = 0.1)
intervalo
funcionfitness <- function(f) rmse(f(intervalo), sin(intervalo))
#Esta funci??n, me dar?? los valores de fitness para cada valor en mi intervalo, y se puede utilizar cualqueir clase de funci??n, incluso algunas ya preestablecidas para c??lculo de fitness de alguna poblaci??n
#La ventaja, es que RGP en automatico, hace que los valores de fitness sean los m??s optimizados para trabajar

#Para encontrar expresiones simbolicas de los valores de fitness, se hace lo siguiente:
set.seed(1)
resultados <- geneticProgramming(functionSet = funciones, inputVariables = variables, constantSet = constantes, fitnessFunction = funcionfitness, stopCondition = makeTimeStopCondition(300))
#Esto arroja todos los valores de fitness asociados a todas funciones posibles de nuestro set de datos
#El tiempo l??mite ayuda a que solo busque durante ese tiempo, porque se podr??a explayar y durar mucho m??s
#Esto dar?? varios, si no miles de resultados, por lo que para ver el mejor se hace lo siguiente:

solucion <- resultados$population[[which.min(resultados$fitnessValues)]]
#Esto arroja, dentro de mi poblaci??n (mis datos), qu?? modelo se ajusta mejor, de acuerdo al menor valor de fitness

#Una vez encontrado el mejor resultado, se puede graficar comparado con la funci??n original, que fue la que se modific?? para ajustarla mejor a los datos
plot(y=solucion(intervalo), x= intervalo, lty = 1)
lines(y = sin(intervalo), x = intervalo, lty = 2)

#REGRESI??N SIMB??LICA
#La regresi??n simb??lica implica sacar un modelo de una serie de datos
#La ecuaci??n en la que me voy a basar para esto, es una ecuaci??n b??sica de una din??mica predador-presa, en la que
# X = x(alfa - beta * y)
#Y = -y(gamma - delta*x)

#utilizar?? valores aleatorios para cada par??metro, as?? como para cada x y y, esto porque el usar valores de datasets ya establecidos ocupa mucho poder de c??mputo y tiempo de ejecuci??n
#Primero se intent?? la corrida, usando ??nicamente a las presas, normalizando la poblaci??n de depredadores como y = 1-x

lotkavolterra <- function(alfa = runif(1), beta = runif(1)){
  function(x) x * (alfa- (beta*(x-1)))
 
}
intervalo <- seq(from = 0, to = 1, by= 0.01)
#Se le da un par??metro aleatorio a un par??metro
#En este caso, se le dio el valor de 1 a alfa, lo que indicar??a un fitness perfecto en la presa, es decir, un crecimiento lineal de la misma

presa <- lotkavolterra(alfa=1)
#Gr??fica de la funcion en ese intervalo de 0 a 1, es decir... como se comportar??a la funcion si los datos fueran normales, y el ajuste perfecto
plot(y=presa(intervalo), x = intervalo, lty=1, xlab = "t", ylab = "presa")

datos <- runif(100, 0.0, 1.0)
lotkacondatos <- data.frame(time = datos, poblacion = presa(datos)+rnorm(length(datos), sd= 0.01))

#Se ajusta ese modelo, mediante regresi??n simb??lica, el parametro final indica que a los dos minutos detendr?? la b??squeda del mejor modelo
modelo <- symbolicRegression(poblacion~time, data = lotkacondatos, stopCondition = makeTimeStopCondition(2*60))

mejormodelo <- modelo$population[[which.min(modelo$fitnessValues)]]
plot(y=mejormodelo(datos), x=datos, type="l", lty= 1, xlab="tiempo", ylab="poblacion presas")
lines(y=presa(datos), x = datos, lty= 2)

#Ese es el mejor ajuste del modelo para presas, ahora haremos lo mismo, pero para la ecuaci??n de depredadores

lotkavolterra2 <- function(gamma = runif(1), delta = runif(1)){
  function(y) -y * (gamma - (delta*(y-1)))
  
}

intervalo2 <- seq(from = 0, to = 1, by= 0.01)
#al igual que con las presas, se le da un valor m??ximo a lo que define el crecimiento lineal
depredador <- lotkavolterra2(delta=1)
plot(y=depredador(intervalo2), x = intervalo2, lty=1, xlab = "t", ylab = "depredador")

datosd <- runif(100, 0.0, 1.0)
lotkacondatos2 <- data.frame(time = datosd, poblacion = depredador(datosd)+rnorm(length(datosd), sd= 0.01))
modelo2 <- symbolicRegression(poblacion~time, data = lotkacondatos2, stopCondition = makeTimeStopCondition(2*60))
mejormodelo2 <- modelo2$population[[which.min(modelo2$fitnessValues)]]
plot(y=mejormodelo2(datosd), x=datosd, type="l", lty= 1, xlab="tiempo", ylab="poblacion depredadores")
lines(y=depredador(datosd), x = datosd, lty= 2)
#Se puede ver como ambas poblaciones se comportan de maneras muy diferentes