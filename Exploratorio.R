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

crudos <- read_excel("CJB_Datos concurso jovenes de bioestadistica.xlsx")

summary(crudos)

#veo si esta balanceado entre tratamientos
table(crudos$Año)
table(crudos$Localidad)
table(crudos$Línea)
# Cruce Línea x Localidad x año 
xtabs(~ Año + Localidad + Línea, data = crudos)
#Esta super balanceado jaja quien puediera. hay 3 "lotes" por linea por localidad por año.

####    VARIABLES  ####
#Hay una banda de variables muchas surgen de cuentas hechas a partir de otras, en el doc hay un glosario de variables
#A grandes razgos los aspectos q se evaluaron fueron: 
  #Pl/m: Plántulas por metro lineal de surco: Entiendo que se refiere a la cantidad de plantulas q se pusieron en un principio en cada sitio, en dicho caso tiene sentido que RELATIVICEMOS todos los resultados a la cantidad inicial de plantulas de cada sitio!
Plantulas <- crudos %>%
  group_by(Año, Localidad, Línea) %>%
  summarise(Pl_m = mean(`Pl/m`, na.rm = TRUE)) # Es una paja que la variable tenga / habria que sacarsela a todas las varibles, podemos poner _ en su lugar
print(Plantulas)

#alv faltan un monton de datos, faltan todos los de 2024 - 2025, ademas no todas las lineas tienen las mismas plantulas iniciales asique vamos a tener que relativizar!
tabla_valores <- Plantulas %>%
  select(Año, Localidad, Línea, Pl_m) %>%
  pivot_wider(
    names_from = Localidad,
    values_from = Pl_m
  )

  #Produccion de semillas: en esto hay algo interesante, parece q no todas las semillas producidas son "verdaderas" hay semillas llenas y semillas vacias, asique estimo que lo importante es q haya muchas llenas y no importa tanto el resto.
  #Macollos: es tipo la cantidad de pastitos que salen por planta, pueden ser de hojas o reproductivos.
            #DMT: densidad total de macollos. 
            #DMR: densidad de macollos reproductivos
            #MR: Macollos reproductivos/macollos totales
#PF: produccion de forrale en gramos de matria seca por metro lineal de surco por corte (hay 4 cortes)
    #Aca hay algo interesante, lo relativizan a distintas variables ambientales
            #TCºCd: Tasa de acumulación de forraje expresada en gramos de materia seca por unidad de tiempo térmico (grado centígrado día), 
            #TC.mm: Tasa de acumulación de forraje expresada en gramos de materia seca por mm de lluvia
            #TC.d: Tasa de acumulación de forraje expresada en gramos de materia seca por día
    #Despues esta lo mismo pero en lugar de por corte es por toda la temporada (o eso entendi)

#La parte de divisores no es muy clara la verdad, pense q eran cuales eran las variables mas usadas para medir tasas de crecimiento pero hmmm permitime dudar.
#MAs que nada me hace ruido el que dice "dias a PF total" tipo porque la cantidad de dias seria informacion de rendimiento??


#Que variables tendriamos que analizar segun la pregunta en juego:
#"Preguntas:
#  1.	¿Existen diferencias significativas entre híbridos en la producción total y en su distribución a lo largo del tiempo?
  
#  2.	¿Existen diferencias significativas entre localidades en la producción total y en su distribución temporal?
  
#  3.	¿Existe interacción significativa entre genotipo y ambiente (G×E)?
  
#  4.	¿Qué variables explican mejor la producción total de semillas?
  
#  5.	¿El uso de tasas de acumulación (por día, grado-día o mm de precipitación) permite corregir las diferencias entre frecuencias de cortes (periodo de tiempo transcurrido entre cortes sucesivos) y diferencias ambientales entre localidades?
  
#  6.	¿Existen diferencias entre híbridos en la densidad de inflorescencias, la producción de semillas y el porcentaje de llenado?
  
#  7.	¿Existen diferencias entre localidades en la densidad de inflorescencias, producción de semillas y porcentaje de llenado?
  
#  8.	¿Cuál sería el mejor híbrido para su registro como cultivar, considerando productividad, estabilidad y reproducción?
  
#  9.	¿Cuál es la localidad más adecuada para la producción comercial de semillas del nuevo cultivar?
  
#  10.	¿Cómo afecta la falta de datos (por ejemplo, el corte 3 del 2o año en Colonia Caroya) en los resultados del análisis? ¿Qué métodos podrían aplicarse para mitigar su impacto?


###Comienzo analisis exploratorio en orden de las preguntas  
#  1.	¿Existen diferencias significativas entre híbridos en la producción total y en su distribución a lo largo del tiempo?
      # Osea vamos a ver cuanto produce cada hibrido en cada luagar en cada temporada y tambien ver en q corte es mayor la produccion

#configuro los datos para hacer analisis exploratorios
datos_largos <- crudos %>%
  pivot_longer(
    cols = starts_with("PF "),
    names_to = "Corte",
    values_to = "PF",
    names_pattern = "PF (.*)"
  ) %>%
  filter(!is.na(PF), 
         !Corte %in% c("Total", "total.ºC", "total.mm", "total.d"))

ggplot(datos_largos, aes(x = Corte, y = PF, fill = Línea)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~Línea) +
  theme_minimal() +
  labs(y = "Producción de forraje (g MS/m)", x = "Corte")
#los boxplots muestran distribucion bastante normal
ggplot(datos_largos, aes(x = Corte, y = PF, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(y = "Producción de forraje (g MS/m)", x = "Corte") 
# En general sin ver el detalle de los distintos lugares:
#    L37 tiene un comportamiento distinto al resto de las lineas, mayor prodcuddion en el segundo corte
#    K 14 tiene misma produccion que J7 en corte 1 y una mayor produccion que el resto hacia el 4to corte. y similar a L34 en el tercer corte.
#   UF93 es la peor de todas


## a chequear, puse solo años 23-24 y relativizado a cantidad inicial de plantulas
datos_largos <- crudos %>%
  pivot_longer(
    cols = starts_with("PF "),
    names_to = "Corte",
    values_to = "PF",
    names_pattern = "PF (.*)"
  ) %>%
  filter(!is.na(PF), 
         !Corte %in% c("Total", "total.ºC", "total.mm", "total.d")) %>%
  filter(Año == "23-24") %>%                      # solo ciclo 23-24
  mutate(PF_rel = PF / `Pl/m`)                    # relativizado por plantas/m

# Boxplots relativizados
ggplot(datos_largos, aes(x = Corte, y = PF_rel, fill = Línea)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~Línea) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", x = "Corte")

# Línea con medias relativizadas
ggplot(datos_largos, aes(x = Corte, y = PF_rel, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", x = "Corte")

# k es el mejor despues J , L y ultimo U
