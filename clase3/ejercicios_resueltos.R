### 1a. Importar tidyverse

library(tidyverse)

### 1b. Importar la base de datos llamada recorridos_bici.csv, que contiene recorridos realizados con bicicletas del GCBA en 2017.

data <- read_csv("data/recorridos_bici.csv")

### 2. Crear un objeto que contenga únicamente 4 variables a elección de la base de datos. 

seleccion_1 <- data %>% select(genero_usuario, nombre_estacion_origen, nombre_estacion_destino, duracion_recorrido)

### 3. Crear un objeto que contenga unicamente los recorridos de mujeres y tres estaciones de origen. 

filtrado_1 <- data %>% filter(genero_usuario == "F" & nombre_estacion_origen %in% c("Hospital Ramos Mejía", "Hospital Garrahan", "Hospital Británico"))

### 4. Crear una nueva columna llamada "rango_horario" que tenga el valor "Mañana" para los recorridos que empezaron en la mañana 
# y "Tarde" para los recorridos que empezaron en la tarde (después de las 12h). 

data <- data %>% mutate(rango_horario = case_when(
  hora_salida < 12 ~ "Mañana", 
  hora_salida >= 12 ~ "Tarde"
))

### 5. Eliminar del dataframe las columnas con infromación de latitud y longitud. 

data_filtrada <- data %>% filter(is.na(lat_estacion_destino) & is.na(long_estacion_destino))

### 6. Convertir los NA de las columnas "domicilio_estacion_origen" y "domicilio_estacion_destino" 
## en un "Sin información" 

data <- data %>% mutate_at(vars(domicilio_estacion_origen, domicilio_estacion_destino), ~case_when(
  is.na(.) ~ "Sin información",
  TRUE ~ .)
  )

### 7. Pasar los valores de genero_usuario 
## a minúscula con ayuda de 
## la función str_to_lower(). 

data <- data %>% mutate(genero_usuario = str_to_lower(genero_usuario))

### 8. Guardar como .csv la última base generada. 

write_csv(data, "data/recorridos_bici_editada.csv")
  
### 9. Instalar un paquete que vamos a usar la próxima clase: lubridate. 

install.packages("lubridate")