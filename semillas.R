
########## SEMILLLAS ###############
library(dplyr)
library(purrr)
library(ggplot2)
library(MuMIn)
library(tidyr)
library(tidyverse)
library(broom)
library(lme4)
library(scales)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(readxl)


#  5.	¿Existen diferencias entre híbridos en la densidad de inflorescencias, producción de semillas y porcentaje de llenado de semillas?
"Medio que ya lo hicimos antes, habria q ver de encarar huntas las preguntas y despues ir al detalle y ya",
#  6.	¿Existen diferencias entre las localidades en la densidad de inflorescencias, producción de semillas y porcentaje de llenado de semillas?
"same shit",
#  7.	Cuál sería el mejor híbrido y cuál sería la localidad más apropiada para la producción de semillas?
"aca conclu con las dif sig etc no es la combinacion!" # tener en cuenta cuantas de las semillas dieron plantulas?
)

#Macollos
ggplot(crudos, aes(x = Línea, y = DMT, fill = Localidad)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(y = "DMT", 
       x = "Línea",
       title = "Macollos por metro") +
  theme_bw()

#Macollos#  DMT: Densidad de macollos totales por metro lineal de surco / Pl/m: Plántulas por metro lineal de surco

datos_ratio <- crudos %>%
  mutate(Macollos_por_pl = DMT / `Pl/m`)

ggplot(datos_ratio, aes(x = Línea, y = Macollos_por_pl, fill = Localidad)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(y = "Macollos por plántula (DMT / Pl_m)", 
       x = "Línea",
       title = "Relación DMT/Plántulas por Línea y Localidad") +
  theme_bw()

#Cuantos son reproductivos# 
ggplot(datos_ratio, aes(x = Línea, y = `%MR`, fill = Localidad)) +
  geom_boxplot(position = position_dodge(width = 0.8)) +
  labs(y = "% Macollosreproductivos", 
       x = "Línea",
       title = "% Macollos reproductivos por Línea y Localidad") +
  theme_bw()

#cuantas semillas produce#



#semillas llenas#