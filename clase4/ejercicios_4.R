#####Paquetes y bases necesarias para la ejercitación######

library(tidyverse)
library(eph)
library(openxlsx)


base <- eph::get_microdata(year = 2019, #definimos el año
                           trimester = 1, #definimos el trimestre
                           type = "individual", #definimos el tipo de base (el INDEC brinda la base a nivel hogar y a nivel individual)
                           destfile = "./eph_2019_T1.rds") %>%  #definimos dónde se va a guardar la base. De esta forma, cada vez que utilicemos este script no tenemos que volver a descargar la base
  select(ESTADO,REGION,"SEXO" = CH04, "ESTADO_CIVIL" = CH07, PONDERA) %>% 
  
  mutate(ESTADO = case_when(ESTADO == 1 ~ "Ocupado",
                            ESTADO == 2 ~ "Desocupado",
                            ESTADO == 3 ~ "Inactivo",
                            ESTADO == 4 ~ "Menor de 10 años"),
         
         REGION =  case_when(REGION == 1 ~ "Gran Buenos Aires",
                             REGION == 40 ~ "Noroeste",
                             REGION == 41 ~ "Nordeste",
                             REGION == 42 ~ "Cuyo",
                             REGION == 43 ~ "Pampeana",
                             REGION == 44 ~ "Patagónica"),
         
         ESTADO_CIVIL = case_when(ESTADO_CIVIL == 1 ~ "Unido",
                                  ESTADO_CIVIL == 2 ~ "Casado",
                                  ESTADO_CIVIL == 3 ~ "Separado/Divorciado",
                                  ESTADO_CIVIL == 4 ~ "Viudo",
                                  ESTADO_CIVIL == 5 ~ "Soltero"),
         
         SEXO = case_when(SEXO == 1 ~ "Varones",
                          SEXO == 2 ~ "Mujeres"))

head(base,50)



######Ejercicio 1:######

#Aclaración: la tasa de actividad se obtiene como cociente entre la población económicamente activa (es decir, ocupada o desocupada) y la población total.

#####a. Construya un dataframe tabla_1 con la tasa de actividad por region.

#####b. Construya un dataframe tabla_2 con la tasa de actividad por region y estado civil. En su versión final, la tabla_2 debería presentar una columna por estado civil y una fila por región.


######Ejercicio 2: ###########

#Construya un dataframe tabla_3 a partir de tabla_1 y tabla_2 (use el join que le parezca adecuado), donde pueda observarse la tasa de actividad para la población total de cada región, así como también desagregada por estado civil.
#Para presentar la información de forma clara, quizás tenga que cambiar el nombre de alguna columna.

######Ejercicio 3: #########

#####a. Construya un dataframe tabla_4 con la tasa de actividad por sexo y estado civil. Reacomode la tabla de manera tal que se presente una columna por estado civil y una fila por sexo

#####b. A partir de la tabla_4, genere una nueva columna donde se presente la diferencia entre la tasa de actividad de la población casada y la población soltera. Compare los resultados obtenidos para cada sexo.

#####c. Guarde la última versión de la tabla_4 en un archivo de excel: ejercicio_4.xlsx
