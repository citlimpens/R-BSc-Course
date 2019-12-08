
setwd("/Users/Cit/Downloads")
getwd()
nombres <- c("G??nero", "Longitud", "Di??metro", "Altura", "Pesostot", "PesoB", "PesoC", "cap", "anillos")
aba <- read.csv("ABALONE.data.txt", header = FALSE, as.is=T)
str(aba)
colnames(aba) <- nombres
str(aba)
#Resumen numerico y grafico
summary(aba)

grupos <- list()
for (gen in c("M", "F")) grupos[[gen]] <- which(aba[,1] == gen)

abam <- aba[grupos$M,]
abaf <- aba[grupos$F,]
abai <- aba[grupos$I,]
str(abam)

summary(abam)
summary(abaf)

summary(abam$Longitud)
summary(abaf$Longitud)
sd(abam$Longitud)
boxplot(abam$Longitud)
shapiro.test(abam$Longitud)

