library(eph)
library(tidyverse)
library(grid)

base_individual <- get_microdata(year = 2021, trimester = 3, type = "individual")

base_individual$P21[is.na(base_individual$P21)] <- 0
base_individual$TOT_P12[is.na(base_individual$TOT_P12)] <- 0
base_individual$PP3E_TOT[is.na(base_individual$PP3E_TOT)] <- 0
base_individual$PP3F_TOT[is.na(base_individual$PP3F_TOT)] <- 0

tabla_resumen <- base_individual %>% 
  filter(ESTADO == 1) %>% 
  mutate(Sexo = as.character(CH04),
         Sexo = case_when(Sexo=="1" ~ "Varones",
                          Sexo=="2" ~ "Mujeres"),
         Ingreso_Laboral_Total = P21 + TOT_P12,
         Horas_totales = PP3E_TOT+PP3F_TOT) %>% #pondiio
  group_by(Sexo) %>% 
  summarise(Ing_Lab = weighted.mean(Ingreso_Laboral_Total, as.numeric(PONDIIO)),
            Horas = weighted.mean(Horas_totales, PONDIIO))


no_bin <- tibble(Sexo = "No binario", Ing_Lab = NA,Horas = NA)

tabla_resumen <- bind_rows(tabla_resumen,no_bin)


tabla_resumen_seg <- tabla_resumen %>% 
  pivot_wider(names_from = "Sexo", values_from = c("Ing_Lab","Horas"))


ggplot(tabla_resumen)+
  geom_point(tabla_resumen,mapping =aes(x = Horas, y = Ing_Lab, color = Sexo), size = 4.5, alpha = .7, show.legend = TRUE)+
  scale_color_manual(values=c("darkorchid1","Orange", "forestgreen"))+
  geom_segment(aes(x = Horas_Mujeres, y = Ing_Lab_Mujeres,
                   yend = Ing_Lab_Mujeres, xend = Horas_Varones),
               data = tabla_resumen_seg,
               alpha = .2,
               linetype = "dashed"
                 )+
  geom_segment(aes(x = Horas_Varones, y = Ing_Lab_Mujeres,
                   yend = Ing_Lab_Varones, xend = Horas_Varones),
               data = tabla_resumen_seg,
               alpha = .2,
               linetype = "dashed")+
  geom_segment(aes(x = Horas_Mujeres + 1 , y = Ing_Lab_Mujeres + 800,
                 yend = Ing_Lab_Mujeres + 800, xend = Horas_Mujeres +4),
             data = tabla_resumen_seg,
             alpha = .2,
             arrow = arrow(length = unit(0.05, "inches"),
                           ends = "both")
  )+
  geom_segment(aes(x = Horas_Varones -4 , y = Ing_Lab_Mujeres + 800,
                   yend = Ing_Lab_Mujeres + 800, xend = Horas_Varones -1),
               data = tabla_resumen_seg,
               alpha = .2,
               arrow = arrow(length = unit(0.05, "inches"),
                             ends = "both")
  )+
  annotate("text", y = tabla_resumen_seg$Ing_Lab_Mujeres + 1100, x = tabla_resumen_seg$Horas_Mujeres+0.2,
           label = paste0("Mujeres"),
           size = 3,
           color = "darkorchid1")+
  annotate("text", y = tabla_resumen_seg$Ing_Lab_Varones - 500, x = tabla_resumen_seg$Horas_Varones - 1.5,
           label = paste0("Varones"),
           size = 3,
           color = "forestgreen")+
  annotate("text", y = tabla_resumen_seg$Ing_Lab_Mujeres + 800, x = (tabla_resumen_seg$Horas_Mujeres + tabla_resumen_seg$Horas_Varones)/2,
             label = paste0("Trabajo no remunerado"),
           size = 2.5)+
  labs(y = "Ingresos laborales totales ($)",
       x = "Horas semanales totales dedicadas al trabajo remunerado",
       title = "",
       caption = "Fuente: EPH - 3° Trimestre 2021")+
  theme_minimal()+
  theme(text = element_text(size = 8),
        legend.position = "top") +
  ggsave("clase1/Intro teorica/prueba_objetividad.png")


ggplot(tabla_resumen)+
  geom_point(tabla_resumen,mapping =aes(x = Horas, y = Ing_Lab, color = Sexo), size = 4.5, show.legend = TRUE)+
  annotate("text", y = tabla_resumen_seg$Ing_Lab_Mujeres + 1000, x = tabla_resumen_seg$Horas_Mujeres+0.5,
           label = paste0("Mujeres"),
           size = 3,
           color = "pink")+
  annotate("text", y = tabla_resumen_seg$Ing_Lab_Varones - 500, x = tabla_resumen_seg$Horas_Varones - 1.5,
           label = paste0("Varones"),
           size = 3,
           color = "light blue")+
  labs(y = "Ingresos laborales totales ($)",
       x = "Horas semanales totales trabajadas",
       title = "",
       caption = "Fuente: EPH - 3° Trimestre 2021")+
  theme_minimal()+
  theme(text = element_text(size = 8),
        legend.position = "top") +
  ggsave("clase1/Intro teorica/contraejemplo_objetividad.png")
    