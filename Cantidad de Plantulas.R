
#      ANALISIS CANTIDAD DE PLANTULAS   #

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
library(car)

crudos <- read_excel("CJB_Datos concurso jovenes de bioestadistica.xlsx")
crudos$Bloque<- as.factor(crudos$Bloque)


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
  theme(legend.position = "bottom")


###           MODELO          ###
####### Modelo plantulas por linea y lugar ###
#VEF: Linea y lugar 
#VR: Cantidad de plantulas (es un conteo-> poisson) !! quiza es normal porque es plantulas por metro lineal chequearr


M_Plantulas <- glm(`Pl/m` ~ Localidad * Línea,
                   family = poisson(link = "log"),
                   data = crudos)
Anova(M_Plantulas, type = 3) # todo super significativo

# líneas dentro de cada localidad
emm_lineas <- emmeans(modelo_pois, ~ Línea | Localidad, type = "response")
pairs(emm_lineas)

# localidades dentro de cada línea
emm_localidades <- emmeans(modelo_pois, ~ Localidad | Línea, type = "response")
pairs(emm_localidades)