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

# Paso a factor las variables que crrespondan
crudos$Bloque<- as.factor(crudos$Bloque)
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
summary(Plantulas)
#alv faltan un monton de datos, faltan todos los de 2024 - 2025, ademas no todas las lineas tienen las mismas plantulas iniciales asique vamos a tener que relativizar!
tabla_valores <- Plantulas %>%
  select(Año, Localidad, Línea, Pl_m) %>%
  pivot_wider(
    names_from = Localidad,
    values_from = Pl_m
  )

#### HAGO PLANTAS RELATIVIZADAS ####

# 1) Tomamos como referencia la densidad de plantas (Pl/m) de 23-24
plantas_ref <- crudos %>%
  filter(Año == "23-24") %>%
  group_by(Localidad, Línea, Bloque) %>%
  summarise(Pl_m_ref = mean(`Pl/m`, na.rm = TRUE), .groups = "drop")

# 2) Creamos la base 'relativos'
relativos <- crudos %>%
  left_join(plantas_ref, by = c("Localidad", "Línea", "Bloque")) %>%
  mutate(
    # usamos siempre Pl/m de 23-24 como denominador
    Pl_m_usada = ifelse(Año == "24-25", Pl_m_ref, `Pl/m`)
  ) %>%
  # 3) Dividimos todas las variables numéricas (excepto Pl/m y las auxiliares)
  mutate(across(
    where(is.numeric) & !c("Pl/m","Pl_m_ref","Pl_m_usada"),
    ~ .x / Pl_m_usada
  )) 
  # 4) Limpiamos auxiliares si no se quieren guardar
  #select(-Pl_m_ref, -Pl_m_usada)


###### TABLAS RESUMEN ######
library(dplyr)

# Preparar dataset con indicadores
datos_resumen <- relativos %>%
  mutate(
    plantulas_x_gr = `Pl/m`,           # ya es por gramo
    macollos = DMT,                    # macollos por plántula
    porc_macollos_reprod = `%MR`,      # porcentaje
    semillas_x_plantula = `Prod. Sem`, # ya relativizado
    porc_semillas_llenas = `%llenado`,
    pf_total = `PF Total`              # producción de forraje total
  )

# ---- Resumen por Línea ----
tabla_linea <- datos_resumen %>%
  group_by(Línea) %>%
  summarise(
    `Plántulas/g (mean ± sd)` = paste0(
      round(mean(plantulas_x_gr, na.rm = TRUE), 2), " ± ",
      round(sd(plantulas_x_gr, na.rm = TRUE), 2)
    ),
    `Macollos (mean ± sd)` = paste0(
      round(mean(macollos, na.rm = TRUE), 2), " ± ",
      round(sd(macollos, na.rm = TRUE), 2)
    ),
    `% macollos reproductivos (mean ± sd)` = paste0(
      round(mean(porc_macollos_reprod, na.rm = TRUE), 2), " ± ",
      round(sd(porc_macollos_reprod, na.rm = TRUE), 2)
    ),
    `Semillas/plántula (mean ± sd)` = paste0(
      round(mean(semillas_x_plantula, na.rm = TRUE), 2), " ± ",
      round(sd(semillas_x_plantula, na.rm = TRUE), 2)
    ),
    `% semillas llenas (mean ± sd)` = paste0(
      round(mean(porc_semillas_llenas, na.rm = TRUE), 2), " ± ",
      round(sd(porc_semillas_llenas, na.rm = TRUE), 2)
    ),
    `PF Total (mean ± sd)` = paste0(
      round(mean(pf_total, na.rm = TRUE), 2), " ± ",
      round(sd(pf_total, na.rm = TRUE), 2)
    )
  )

# ---- Resumen por Localidad ----
tabla_localidad <- datos_resumen %>%
  group_by(Localidad) %>%
  summarise(
    `Plántulas/g (mean ± sd)` = paste0(
      round(mean(plantulas_x_gr, na.rm = TRUE), 2), " ± ",
      round(sd(plantulas_x_gr, na.rm = TRUE), 2)
    ),
    `Macollos (mean ± sd)` = paste0(
      round(mean(macollos, na.rm = TRUE), 2), " ± ",
      round(sd(macollos, na.rm = TRUE), 2)
    ),
    `% macollos reproductivos (mean ± sd)` = paste0(
      round(mean(porc_macollos_reprod, na.rm = TRUE), 2), " ± ",
      round(sd(porc_macollos_reprod, na.rm = TRUE), 2)
    ),
    `Semillas/plántula (mean ± sd)` = paste0(
      round(mean(semillas_x_plantula, na.rm = TRUE), 2), " ± ",
      round(sd(semillas_x_plantula, na.rm = TRUE), 2)
    ),
    `% semillas llenas (mean ± sd)` = paste0(
      round(mean(porc_semillas_llenas, na.rm = TRUE), 2), " ± ",
      round(sd(porc_semillas_llenas, na.rm = TRUE), 2)
    ),
    `PF Total (mean ± sd)` = paste0(
      round(mean(pf_total, na.rm = TRUE), 2), " ± ",
      round(sd(pf_total, na.rm = TRUE), 2)
    )
  )

# ---- Resumen por Temporada (Año) ----
tabla_temporada <- datos_resumen %>%
  group_by(Año) %>%
  summarise(
    `Plántulas/g (mean ± sd)` = paste0(
      round(mean(plantulas_x_gr, na.rm = TRUE), 2), " ± ",
      round(sd(plantulas_x_gr, na.rm = TRUE), 2)
    ),
    `Macollos (mean ± sd)` = paste0(
      round(mean(macollos, na.rm = TRUE), 2), " ± ",
      round(sd(macollos, na.rm = TRUE), 2)
    ),
    `% macollos reproductivos (mean ± sd)` = paste0(
      round(mean(porc_macollos_reprod, na.rm = TRUE), 2), " ± ",
      round(sd(porc_macollos_reprod, na.rm = TRUE), 2)
    ),
    `Semillas/plántula (mean ± sd)` = paste0(
      round(mean(semillas_x_plantula, na.rm = TRUE), 2), " ± ",
      round(sd(semillas_x_plantula, na.rm = TRUE), 2)
    ),
    `% semillas llenas (mean ± sd)` = paste0(
      round(mean(porc_semillas_llenas, na.rm = TRUE), 2), " ± ",
      round(sd(porc_semillas_llenas, na.rm = TRUE), 2)
    ),
    `PF Total (mean ± sd)` = paste0(
      round(mean(pf_total, na.rm = TRUE), 2), " ± ",
      round(sd(pf_total, na.rm = TRUE), 2)
    )
  )



#### Exploratorio de cantidad de plantulas q crecieron en cada lugar con cada linea ####
datos_plot <- crudos %>%
  filter(!is.na(`Pl/m`)) %>%
  group_by(Localidad, Línea) %>%
  summarise(
    mean_pl = mean(`Pl/m`, na.rm = TRUE),
    se_pl = sd(`Pl/m`, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

ggplot(datos_plot, aes(x = Localidad, y = mean_pl, fill = Línea)) +
  # puntos crudos alineados
  geom_point(data = crudos %>% filter(!is.na(`Pl/m`)),
             aes(x = Localidad, y = `Pl/m`, color = Línea),
             position = position_dodge(width = 0.8),
             size = 2, alpha = 0.8, inherit.aes = FALSE) +
  labs(x = "Localidad", y = "Plántulas por metro",
       title = "Cantidad de plántulas por línea en cada localidad") +
  # barras de medias
  geom_col(position = position_dodge(width = 0.8), alpha = 0.7) +
  # barras de error
  geom_errorbar(aes(ymin = mean_pl - se_pl, ymax = mean_pl + se_pl),
                position = position_dodge(width = 0.8), width = 0.2) +
  theme_minimal() +
  theme(legend.position = "bottom") ### El analisis de esto lo continue en un doc aparte llamado "Cantidad de Plantulas.R" ahi esta el modelo y el cheque de sus supuestos


#####

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
#Preguntas: ####
print(
#  1.	¿Existe interacción genotipo*ambiente?
"Serian 5 modelos algunos de medidas repetidas (los cortes) 
VR: semillas llenas , produccion total/ dias a produccion total ,  TC°C 1/2/3/4 y TC mm . CANTIDAD DE PLANTULAS (todas tienen la misma cantidad de semillas)
VEF: Genotipo ambiente Genotipo*Ambiente  # Ver si da significativa la interaccion
VA: Año",

#  2.	¿Existen diferencias entre híbridos en la distribución de la producción de forraje y la producción total acumulada?
"VR: produccion total/ dias a produccion total,   TC dia 1/2/3/4, TC°C 1/2/3/4 y TC mm
VEF: Genotipo  # Ver si da significativa la interaccion
VA: Año, Localidad",

#  3.	¿Existen diferencias entre las localidades en la distribución de la producción de forraje y la producción total acumulada?
"VR: produccion total/ dias a produccion total,   TC dia 1/2/3/4
VEF: Localidad  # Ver si da significativa la interaccion
VA: Año, Genotipo",

#  4.	¿El uso de tasas de acumulación (por día, grado-día o mm de precipitación) permite corregir las diferencias entre frecuencias de cortes (periodo de tiempo transcurrido entre cortes sucesivos) y diferencias ambientales entre localidades?
"Dos ideas: 
  3 Modelos:
    VEF (una por modelo): TC dias, TC mm y TC °C.
    VA: Lugar", # La idea es ver si la varianza explicada por lugar es similar a la explicada por bloque. Dia deberia tener maxima Varianza entre lugares e ir disminuyendo con °C o mm. 
    #Si con °C o mm la varianza entre bloques es similar a entre sitios entonces puedo quedarme tranqui de que mm o °C es una buena forma de relativizar.
    #No estoy segura de que se pueda poner como VA a bloque en dicho caso puedo ver de la varianza relativa Varianza explicada por VA/ varianza residual
    
  
#  5.	¿Existen diferencias entre híbridos en la densidad de inflorescencias, producción de semillas y porcentaje de llenado de semillas?
"Medio que ya lo hicimos antes, habria q ver de encarar huntas las preguntas y despues ir al detalle y ya",
#  6.	¿Existen diferencias entre las localidades en la densidad de inflorescencias, producción de semillas y porcentaje de llenado de semillas?
"same shit",
#  7.	Cuál sería el mejor híbrido y cuál sería la localidad más apropiada para la producción de semillas?
"aca conclu con las dif sig etc no es la combinacion!" # tener en cuenta cuantas de las semillas dieron plantulas?
)
#########  

###Comienzo analisis exploratorio en orden de las preguntas  
##  1.	¿Existen diferencias significativas entre híbridos en la producción total y en su distribución a lo largo del tiempo? ####
      # Osea vamos a ver cuanto produce cada hibrido en cada luagar en cada temporada y tambien ver en q corte es mayor la produccion

#configuro los datos para hacer analisis exploratorios
datos_largos <- relativos %>%
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



## a chequear y relativizado a cantidad inicial de plantulas
datos_largos_PF <- relativos %>%
  pivot_longer(
    cols = c("PF 1", "PF 2", "PF 3", "PF 4"),
    names_to = "Corte",
    values_to = "PF",
    names_pattern = "PF (.*)"
  ) %>%
  mutate(,
    Corte = factor(Corte, levels = c("1","2","3","4"))) %>%
  select(Año, Localidad, Línea, Bloque, Corte, PF)

tablas_PF_por_sitio <- datos_largos_PF %>%
  group_split(Localidad)
tablas_PF_por_sitio
# relativizado por plantas/m


# Boxplots relativizados
ggplot(datos_largos, aes(x = Corte, y = PF, fill = Línea)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~Línea) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", x = "Corte")

# Línea con medias relativizadas
ggplot(datos_largos, aes(x = Corte, y = PF, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", x = "Corte")

# k es el mejor despues J , L y ultimo U


ggplot(datos_largos, aes(x = Corte, y = PF, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", 
       x = "Corte",
       title = "Medias de PF relativa por corte y línea en cada localidad")

## Hay una interaccion genotipo ambiente!
      #Cerro azul: J7
      #Colonia Caroya: K14 (falta el 3 corte en esta locacion)
      #Corrientes: ta peleadoo 
      #Reconquista: K14

### TEMPERATURA: ####
datos_TC <- relativos %>%
  pivot_longer(
    cols = matches("^TC\\d [1-4]$"),    # columnas TC.ºC 1,2,3,4
    names_to = "Corte",
    values_to = "TC"
  ) %>%
  filter(!is.na(TC))%>%
  mutate(                         # relativizado por plantas
    Corte = factor(gsub("TC.ºC ", "", Corte),      # dejar solo número de corte
                   levels = c("1", "2", "3", "4"))  # porque la 4 no sale en los grafico :( ??
  )
ggplot(datos_TC, aes(x = Corte, y = TC, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "TC.ºC relativo (°C acumulados / planta)", 
       x = "Corte",
       title = "Medias de TC.ºC relativo por cortes en cada localidad")
#Si yo hago lo mismo con gramos por °C Las relaciones tienen que ser las mismas!

### TEMPERATURA: ####
datos_TC <- relativos %>%
  pivot_longer(
    cols = matches("^TC\\.ºC [1-4]$"),    # columnas TC.ºC 1,2,3,4
    names_to = "Corte",
    values_to = "TC"
  ) %>%
  filter(!is.na(TC))%>%
  mutate(                         # relativizado por plantas
    Corte = factor(gsub("TC.ºC ", "", Corte),      # dejar solo número de corte
                   levels = c("1", "2", "3", "4"))  # porque la 4 no sale en los grafico :( ??
  )
ggplot(datos_TC, aes(x = Corte, y = TC, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "TC.ºC relativo (°C acumulados / planta)", 
       x = "Corte",
       title = "Medias de TC.ºC relativo por cortes en cada localidad")

#Día####
datos_dia <- relativos %>%
  pivot_longer(
    cols = matches("^TC\\.d [1-4]$"),    # columnas TC.d 1,2,3,4
    names_to = "Corte",
    values_to = "TC"
  ) %>%
  filter(!is.na(TC))%>%
  mutate(                         # relativizado por plantas
    Corte = factor(gsub("TC.d ", "", Corte),      # dejar solo número de corte
                   levels = c("1", "2", "3", "4"))  # porque la 4 no sale en los grafico :( ??
  )
ggplot(datos_dia, aes(x = Corte, y = TC, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "TC.d relativo (TC por día / planta)", 
       x = "Corte",
       title = "Medias de TC.d relativo por cortes en cada localidad")




################################### a borrar codigo viejo!! #################

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
datos_largos_PF <- crudos %>%
  pivot_longer(
    cols = c("PF 1", "PF 2", "PF 3", "PF 4"),
    names_to = "Corte",
    values_to = "PF",
    names_pattern = "PF (.*)"
  ) %>%
  mutate(
    PF_rel = PF / `Pl/m`,
    Corte = factor(Corte, levels = c("1","2","3","4"))) %>%
  select(Año, Localidad, Línea, Bloque, Corte, PF, PF_rel)

tablas_PF_por_sitio <- datos_largos_PF %>%
  group_split(Localidad)
tablas_PF_por_sitio
# relativizado por plantas/m


# Boxplots relativizados
ggplot(datos_largos_PF, aes(x = Corte, y = PF_rel, fill = Línea)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_wrap(~Línea) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", x = "Corte")

# Línea con medias relativizadas
ggplot(datos_largos_PF, aes(x = Corte, y = PF_rel, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", x = "Corte")

# k es el mejor despues J , L y ultimo U


ggplot(datos_largos_PF, aes(x = Corte, y = PF_rel, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "Producción relativa (g MS / planta)", 
       x = "Corte",
       title = "Medias de PF relativa por corte y línea en cada localidad")

## Hay una interaccion genotipo ambiente!
#Cerro azul: J7
#Colonia Caroya: K14 (falta el 3 corte en esta locacion)
#Corrientes: ta peleadoo 
#Reconquista: K14


#Si yo hago lo mismo con gramos por °C Las relaciones tienen que ser las mismas!

#TEMPERATU:
datos_TC <- crudos %>%
  pivot_longer(
    cols = matches("^TC\\.ºC [1-4]$"),    # columnas TC.ºC 1,2,3,4
    names_to = "Corte",
    values_to = "TC"
  ) %>%
  filter(!is.na(TC)) %>%
  filter(Año == "23-24") %>%
  mutate(
    TC_rel = TC / `Pl/m`,                          # relativizado por plantas
    Corte = factor(gsub("TC.ºC ", "", Corte),      # dejar solo número de corte
                   levels = c("1", "2", "3", "4")) 
  )
ggplot(datos_TC, aes(x = Corte, y = TC_rel, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "TC.ºC relativo (°C acumulados / planta)", 
       x = "Corte",
       title = "Medias de TC.ºC relativo por cortes en cada localidad")
#################################










### PRODUCCION DE SEMILLAS ####
pl_ref <- crudos %>% # despues habia que cambiarlo para que use directamente la base de datos relativizadas.
  filter(Año == "23-24") %>%
  distinct(Localidad, Línea, Bloque, .keep_all = TRUE) %>%
  select(Localidad, Línea, Bloque, Pl_m_ref = `Pl/m`)

datos_Semillas <- crudos %>%
  left_join(pl_ref, by = c("Localidad", "Línea", "Bloque")) %>%
  mutate(
    Prod_Sem_rel      = `Prod. Sem` / Pl_m_ref,
    Prod_S_llenas_rel = `Prod. S. llenas` / Pl_m_ref
  )

datos_Semillas<-relativos
Prod_Sem_rel<-relativos$`Prod. Sem`
Prod_S_llenas_rel<-relativos$`Prod. S. llenas`
ggplot(datos_Semillas, aes(x = Localidad, y = Prod_Sem_rel, fill = Línea)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = Línea), size = 2, alpha = 0.8,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  facet_wrap(~Año) +
  labs(title = "Producción de Semillas relativizada (Pl/m 23-24 por Bloque)",
       y = "Prod. Sem / Pl/m (23-24)", x = "Localidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")

ggplot(datos_Semillas, aes(x = Localidad, y = Prod_S_llenas_rel, fill = Línea)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(aes(color = Línea), size = 2, alpha = 0.8,
              position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  facet_wrap(~Año) +
  labs(title = "Producción de Semillas Llenas relativizada (Pl/m 23-24 por Bloque)",
       y = "Prod. Sem / Pl/m (23-24)", x = "Localidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Dark2")
#Macollos####
#Densidad de Macollos, usando los datos crudos

#Según línea
ggplot(crudos, aes(x = Línea, y =`DMR`, fill = Línea)) +
  geom_boxplot() +
  labs(
    title = "Cantidad de Macollor Reproductivos según cantidad de Macollos Totales(DMR) por Línea",
    x = "Línea",
    y = "DMR"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#j7 da más macollos reproductivos (a simple vista)
#Según localidad
ggplot(crudos, aes(x = Localidad, y =`DMR`, fill = Localidad)) +
  geom_boxplot() +
  labs(
    title = "Cantidad de Macollor Reproductivos según cantidad de Macollos Totales(DMR) por Localidad",
    x = "Localidad",
    y = "DMR"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#Colonia Caroya posee un mayor DMR (habría que evaluar si difiere significativamente de Reconquista),
#A notar que Corrientes tiene muy pocos, pese al alto número de plantas (acá no está estandarizado)
#
# LLUVIA ####
datos_mm <- relativos %>%
  pivot_longer(
    cols = matches("^TC\\.mm [1-4]$"),    # columnas TC.mm 1,2,3,4
    names_to = "Corte",
    values_to = "TC"
  ) %>%
  filter(!is.na(TC))%>%
  mutate(                         # relativizado por plantas
    Corte = factor(gsub("TC.mm ", "", Corte),      # dejar solo número de corte
                   levels = c("1", "2", "3", "4"))  # porque la 4 no sale en los grafico :( ??
  )
ggplot(datos_mm, aes(x = Corte, y = TC, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~Localidad) +
  theme_minimal() +
  labs(y = "TC.mm relativo (mm acumulados / planta)", 
       x = "Corte",
       title = "Medias de TC.mm relativo por cortes en cada localidad")

#PRODUCCION TOTAL en cada sitio para cada planta 
#Según linea x localidad
ggplot(relativos, aes(x = Línea, y =`PF Total`, fill = Línea)) +
  geom_boxplot() +
  facet_wrap(~ Localidad) +
  labs(
    title = "Producción relativizada (PFtotal) por Línea en cada Localidad",
    x = "Línea",
    y = "PF total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#Según localidad x línea
ggplot(relativos, aes(x = Localidad, y =`PF Total`, fill = Localidad)) +
  geom_boxplot() +
  facet_wrap(~ Línea) +
  labs(
    title = "Producción relativizada (PFtotal) por Localidad según la Línea",
    x = "Localidad",
    y = "PF total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#Producción por localidad sin usar a las líneas como factor
ggplot(relativos, aes(x = Localidad, y =`PF total.d`, fill = Localidad)) +
  geom_boxplot() +
  labs(
    title = "Producción relativizada (PFtotal.d) por Localidad por día",
    x = "Localidad",
    y = "PF total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
#Producción por linea sin usar las localidades como factor
ggplot(relativos, aes(x = Línea, y =`PF total.d`, fill = Línea)) +
  geom_boxplot() +
  labs(
    title = "Producción relativizada (PFtotal.d) por Línea por día",
    x = "Línea",
    y = "PF total"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

