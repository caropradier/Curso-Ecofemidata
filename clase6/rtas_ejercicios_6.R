#####Paquetes y bases necesarias para la ejercitación######

library(tidyverse)
library(eph)
library(plotly)


base <- eph::get_microdata(year = 2019, #definimos el año
                           trimester = 1, #definimos el trimestre
                           type = "individual", #definimos el tipo de base (el INDEC brinda la base a nivel hogar y a nivel individual)
                           destfile = "clase4/eph_2019_T1.rds") %>%  #definimos dónde se va a guardar la base. De esta forma, cada vez que utilicemos este script no tenemos que volver a descargar la base
  select(ESTADO,"SEXO" = CH04, NIVEL_ED, PONDERA) %>% 
  
  mutate(ESTADO = case_when(ESTADO == 1 ~ "Ocupado",
                            ESTADO == 2 ~ "Desocupado",
                            ESTADO == 3 ~ "Inactivo",
                            ESTADO == 4 ~ "Menor de 10 años"),
         
         NIVEL_ED =  case_when(NIVEL_ED == 1 ~ "Primaria Incompleta",
                               NIVEL_ED == 2 ~ "Primaria Completa",
                               NIVEL_ED == 3 ~ "Secundaria Incompleta",
                               NIVEL_ED == 4 ~ "Secundaria Completa",
                               NIVEL_ED == 5 ~ "Superior Universitaria Incompleta",
                               NIVEL_ED == 6 ~ "Superior Universitaria Completa",
                               NIVEL_ED == 7 ~ "Sin instrucción"),
         
         
         SEXO = case_when(SEXO == 1 ~ "Varones",
                          SEXO == 2 ~ "Mujeres"))

head(base,50)


######Ejercicio 1:######

#Aclaración: la tasa de desempleo se obtiene como cociente entre la población desocupada y la población económicamente activa (es decir, ocupada o desocupada).

#####a. Calcule la tasa de desempleo para la población con Nivel educativo == Secundaria Completa, desagregando por sexo.

t_desesmpleo <- base %>% 
  filter(NIVEL_ED == "Secundaria Completa") %>% 
  group_by(SEXO) %>% 
  summarise(tasa_desempleo = sum(PONDERA[ESTADO %in% c("Desocupado")],na.rm = T)/sum(PONDERA[ESTADO %in% c("Ocupado","Desocupado")],na.rm = T))

####b. Genere una función que permita llevar a cabo este mismo cálculo para cada grupo de población según nivel educativo (es decir, la función debe tener como argumento el Nivel educativo de la población que se desea analizar).

t_desempleo_nivel_ed = function(Nivel_educativo){
  
  t_desesmpleo <- base %>% 
    filter(NIVEL_ED == Nivel_educativo) %>% 
    group_by(SEXO) %>% 
    summarise(tasa_desempleo = sum(PONDERA[ESTADO %in% c("Desocupado")],na.rm = T)/sum(PONDERA[ESTADO %in% c("Ocupado","Desocupado")],na.rm = T))
  
  return(t_desesmpleo)
  
  
}

#Probamos que funcione

t_desempleo_nivel_ed("Superior Universitaria Completa")
t_desempleo_nivel_ed("Sin instrucción")

####c. Cree una lista llamada datos_resultados y, utilizando un loop que recorra el vector de todos los niveles educativos posibles, guarde el procesamiento del ejercicio a para cada nivel educativo en la lista.

vector_niveles_educativos <- unique(base$NIVEL_ED)

datos_resultados <- list()

for (nivel in (seq(length(vector_niveles_educativos)))) {
  
  datos_resultados[[nivel]] <- t_desempleo_nivel_ed(vector_niveles_educativos[nivel])
  
}


#veo un caso 

vector_niveles_educativos[1]
datos_resultados[[1]]


######Ejercicio 2:######

#Cree una visualización interactiva de alguno de los procesamientos guardados en la lista datos_resultados, donde se exprese claramente el universo de población analizado, el indicador propuesto, y el contraste entre los resultados para la población femenina y masculina.

library(wesanderson)

grafico_estatico <- t_desesmpleo %>% 
  ggplot(.,aes(x = SEXO, y = tasa_desempleo, fill= SEXO
               ,text = paste0("<br><b>Secundaria Completa</b>","<br>",SEXO,"<br>Tasa de desempleo: ", round(tasa_desempleo*100,1), "%" )
               ))+
  geom_col()+
  theme_minimal()+
  scale_fill_manual(values = wes_palette("Darjeeling1"))+
  labs(title = str_wrap("Tasas de desempleo por sexo. Población con Secundaria Completa.",30),
       y = "Tasa de desempleo",
       x = "Sexo")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = function(x) paste0(x*100,"%"))
  
ggplotly(grafico_estatico, 
         tooltip = "text") %>% 
  layout(font = list(family ="Times New Roman"))

##Bonus: intente transformar su código de visualización en una función donde el argumento sea el nivel educativo alcanzado, a los fines de poder visualizar con mayor comodidad los resultados para cualquier segmento de la población (pista: los elementos del vector vector_niveles_educativos están en el mismo orden que la lista datos_resultados, indexe!).

visualizador_interactivo = function(Nivel_educativo){
  
  numero_indexador <- which(vector_niveles_educativos ==  Nivel_educativo)
  
  grafico_estatico <- datos_resultados[[numero_indexador]] %>% 
    ggplot(.,aes(x = SEXO, y = tasa_desempleo, fill= SEXO
                 ,text = paste0("<br><b>",Nivel_educativo,"</b>","<br>",SEXO,"<br>Tasa de desempleo: ", round(tasa_desempleo*100,1), "%" )
    ))+
    geom_col()+
    theme_minimal()+
    scale_fill_manual(values = wes_palette("Darjeeling1"))+
    labs(title = str_wrap(paste0("Tasas de desempleo por sexo. Población con ",Nivel_educativo,"."),30),
         y = "Tasa de desempleo",
         x = "Sexo")+
    theme(legend.position = "none")+
    scale_y_continuous(labels = function(x) paste0(x*100,"%"))
  
  ggplotly(grafico_estatico, 
           tooltip = "text") %>% 
    layout(font = list(family ="Times New Roman"))
  
  
}

#prueba

visualizador_interactivo("Superior Universitaria Completa")
visualizador_interactivo("Sin instrucción")
visualizador_interactivo("Secundaria Completa")


