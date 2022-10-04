### Ejercicios clase 2

# 1. Creando valores, vectores y dataframes.

# a. Cree un objeto llamado "primer_objeto" que tenga el resultado de la multiplicación 10 x 7.

primer_objeto <- 10*7

# b. Cree un vector llamado "primer_vector" que tenga los valores 15, 30, 45. 

primer_vector <- c(15, 30, 45)

# c. Agregue un cuarto elemento a "primer_vector" que tenga el valor 60. 

primer_vector[4] <- 60

# d. Cree otros dos vectores, pero con variables de texto, que tengan el mismo largo que nuestro primer vector. 

segundo_vector <- c("Sociología", "Economía", "Sociología", "Ciencia Política")
tercer_vector <- c("Argentina", "Argentina", "Colombia", "Chile")

# e. Cree un dataframe a partir de esos dos vectores. 

dataframe <- data.frame(primer_vector, segundo_vector, tercer_vector)

# f. ¿Qué medidas resumen puede utilizar para ver ese dataframe? ¿Y para ver la estructura? 

summary(dataframe)

str(dataframe)

# g. ¿Qué dimensiones tiene ese dataframe?

dim(dataframe)

# 2. Explorando un dataset de R: Iris. 
# Al instalar **R** cuentan con un set de bases de datos de "juguete" para hacer ciertas pruebas, experimentos, etc. 
# Iris es uno de ellos.

# a. Cree un objeto llamado 'data' que contenga el resultado de ejecutar iris

data <- iris

# b. Teniendo en cuenta que iris es una función, ¿qué comando puedo utilizar para que R me devuelva información del dataset? 

?iris

# c. Imprima las primeras 20 filas de iris. 

head(data, n=20)

# d. ¿Cuáles son los valores únicos de la columna "Species"? 

unique(data$Species)

# e. Cree un nuevo dataframe llamado 'setosa' que contenga otdas las filas cuya especie es Setosa. 

setosa <- data[data$Species == "setosa",]

# 3. Preparándonos para la próxima clase.
# En este ejercicio, vamos a ver como se instalan paquetes nuevos en R instalando tidyverse, el cual utilizaremos el resto del curso.
# Agreguen dentro del paréntesis de esta función la siguiente palabra: "tidyverse"(con comillas) y corran el código.

install.packages(tidyverse)
