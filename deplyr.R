install.packages("dplyr")
library(deplyr)
#Filtrar filas (datos de sesion7):, similar a lo que hicimos con abalone
filter()
#Los que no existen, vienen como ?, entonces se le dice que los lea como NA
#Para arreglar base de datos, se pueden arreglar de acuerdo a variables:
arrange(basededatos, variables (pueden ser una, dos, las que se quieran))#
#por defecto lo ordena de menor a mayor, reverso con desc
#Para seleccionar una fila y hacerle algo
select()
#Para extraer patrones (valores ??nicos) para las variables
distinct()
#Se pueden crear nuevas variables a partir de las existentes
#Se pueden aplicar funciones a las columnas tambi??n
#Se puede separar la tabla que se vaya a usar para llevar la tabla que se quiera a un txt. se puede separar como se quiera
