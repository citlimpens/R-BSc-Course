datos_libres <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
datos_pima <- "https://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data"

datos <- read.table (datos_pima, header = FALSE, sep = ",")
#str = structure
str(datos)

nombres <- c("pregnant", "glucose", "diastolic", "triceps", "insulin", "bmi", "diabetes", "age", "test")
#Para cambiar un valor a otro
datos$V2[datos$V2==2] <- 3

colnames(datos) <- nombres
attach(datos)
hist(pregnant)
hist(glucose)
hist(diastolic)

summary(datos)
#Existem datos en los que hay valores con 0 que no pueden ser

sort(datos$glucose)
sort(datos$diastolic)
sort(datos$triceps)
sort(datos$insulin)
sort(datos$bmi)

datos$glucose[datos$glucose==0] <- NA
datos$diastolic[datos$diastolic==0] <- NA
datos$triceps[datos$triceps==0] <- NA
datos$insulin[datos$insulin==0] <- NA
datos$bmi[datos$bmi==0] <- NA

#Test debe cambiarse a binomial
datos$test <- factor(datos$test)
table(datos$test)
levels(datos$test) <- c("positive", "negative")
boxplot(datos$glucose~datoss$test main= "Boxplot for glucose", xlab = "group", ylab = "concentration")
qqnorm(datos$glucose[datos$test=="negative"], na.rm=TRUE)
qqnorm(datos$glucose[datos$test=="positive"], na.rm=TRUE)

qqnorm(datos$bmi[datos$test=="negative"], na.rm=TRUE)
cor.test(datos$pregnant,datos$glucose)
cor.test(datos$diastolic,datos$glucose)
cor.test(datos$triceps,datos$glucose)
cor.test(datos$insulin,datos$glucose)
cor.test(datos$bmi,datos$glucose)
cor.test(datos$diabetes,datos$glucose)
cor.test(datos$age,datos$glucose)
cor.test(datos$pregnant,datos$diastolic)
cor.test(datos$pregnant,datos$triceps)
cor.test(datos$pregnant,datos$insulin)
cor.test(datos$pregnant,datos$bmi)
corrgram(datos$pregnant,datos$age)



#Qu?? tan eficientes son estas variables para predecir
#Selecci??n de modelos - criterio de akaike
#cuando se tienen variables no independeintes = an??lisis de componentes principales, apra hacer regresi??n multple con ellos
#Regresi??n cresta
#Matriz de graficos
#Componentes principales = primero se tienen que eliminar los valores que tengan NAs
#Promcomp con forma de formula, se le pondr??a el na.omit dentro de la formula

princomp(~pregnant+glucose+diastolic, cor = TRUE, na.omit=TRUE)

?princomp

datos$test

testpos <- datos[test$0]
for(i in 1:col(test))




