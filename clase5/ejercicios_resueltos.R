# 1. Importen la librería tidyverse.

library(tidyverse)

# 2. Importen la base de datos actualizada de precios de Menstruacción que está en la carpeta data. 

menstru_marzo_2022 <- read_csv("data/menstru_marzo_2022.csv")

# 3. Con un gráfico de barras, respondan a la pregunta: ¿qué tipo de producto tiene mayor cantidad de registros? 

ggplot(menstru_marzo_2022, aes(x = Categoría))+
  geom_bar()

# 4. ¿Qué marca tiene mayor cantidad de productos? Respondan a la pregunta agregándole otro atributo estético al gráfico anterior. 

ggplot(menstru_marzo_2022, aes(x = Categoría, fill = Marca))+
  geom_bar(position = 'dodge')

# 5. En un gráfico, exprese la mediana, rangos intercuartílicos y valores máximos y mínimos de precios en cada región. 
# ¿En qué región los prodctos menstruales tienen mayor costo? 
# Rta: En el NOA.

ggplot(menstru_marzo_2022, aes(x = Region, y = precio_unidad))+
  geom_boxplot()
