#####Paquetes y bases necesarias para la ejercitación######

library(tidyverse)
library(eph)
library(openxlsx)


base <- eph::get_microdata(year = 2019, #definimos el año
                           trimester = 1, #definimos el trimestre
                           type = "individual" #definimos el tipo de base (el INDEC brinda la base a nivel hogar y a nivel individual)
                           #,destfile = "clase4/eph_2019_T1.rds"
                           ) %>%  #definimos dónde se va a guardar la base. De esta forma, cada vez que utilicemos este script no tenemos que volver a descargar la base
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

tabla_1 <- base %>% 
  group_by(REGION) %>% 
  summarise(tasa_actividad = sum(PONDERA[ESTADO %in% c("Ocupado","Desocupado")],na.rm = T)/sum(PONDERA,na.rm = T))

#####b. Construya un dataframe tabla_2 con la tasa de actividad por region y estado civil. En su versión final, la tabla_2 debería presentar una columna por estado civil y una fila por región.

tabla_2 <- base %>% 
  filter(!is.na(ESTADO_CIVIL)) %>% 
  group_by(REGION,ESTADO_CIVIL) %>% 
  summarise(tasa_actividad = sum(PONDERA[ESTADO %in% c("Ocupado","Desocupado")],na.rm = T)/sum(PONDERA,na.rm = T)) %>% 
  pivot_wider(id_cols = REGION, names_from = ESTADO_CIVIL, values_from = tasa_actividad)

######Ejercicio 2: ###########

#Construya un dataframe tabla_3 a partir de tabla_1 y tabla_2 (use el join que le parezca adecuado), donde pueda observarse la tasa de actividad para la población total de cada región, así como también desagregada por estado civil.
#Para presentar la información de forma clara, quizás tenga que cambiar el nombre de alguna columna.

tabla_3 <- tabla_1 %>% 
  rename("Total" = tasa_actividad) %>% 
  left_join(.,tabla_2, by = "REGION")

######Ejercicio 3: #########

#####a. Construya un dataframe tabla_4 con la tasa de actividad por sexo y estado civil. Reacomode la tabla de manera tal que se presente una columna por estado civil y una fila por sexo

tabla_4 <- base %>% 
  filter(!is.na(ESTADO_CIVIL)) %>% 
  group_by(SEXO,ESTADO_CIVIL) %>% 
  summarise(tasa_actividad = sum(PONDERA[ESTADO %in% c("Ocupado","Desocupado")],na.rm = T)/sum(PONDERA,na.rm = T))%>% 
  pivot_wider(id_cols = SEXO, names_from = ESTADO_CIVIL, values_from = tasa_actividad)

#####b. A partir de la tabla_4, genere una nueva columna donde se presente la diferencia entre la tasa de actividad de la población casada y la población soltera. Compare los resultados obtenidos para cada sexo.

tabla_4 <- tabla_4 %>% 
  mutate("Diferencia (casados-solteros)" = Casado-Soltero)

#####c. Guarde la última versión de la tabla_4 en un archivo de excel: ejercicio_4.xlsx

write.xlsx(tabla_4,"clase4/ejercicio_4.xlsx")
