
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
library(lmerTest)
library(ggplot2)
library(paletteer)
library(emmeans)
library(multcomp)
# Paleta de colores 
cols <- paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]

#  5.	DMR
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
str(crudos)
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

## Hay dif sig entre lineas? #sin relastivizar
DMR_Linea <- lmer(DMR ~ Línea + (1|Localidad) + (1|Año), data = crudos)
summary(DMR_Linea) 
anova(DMR_Linea)  #Si
emm_lineas <- emmeans(DMR_Linea, ~ Línea) #contraste
pairs(emm_lineas, adjust = "tukey") #J7-L37 y J7-UF93
 #grafiquito
emm_lineas <- emmeans(DMR_Linea, ~ Línea) # Medias estimadas de DMR por Línea
cld_lineas <- multcomp::cld(emm_lineas, Letters = letters, adjust = "tukey")
df_plot <- as.data.frame(cld_lineas)

ggplot(df_plot, aes(x = Línea, y = emmean, fill = Línea)) + geom_col(color = "black") + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = .group, y = emmean + 5),   # ajustá el +5 según escala de DMR
            size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(
    x = "Línea",
    y = "Densidad de inflorescencias (DMR, predicho)",
    title = "Medias estimadas de DMR por línea"
  ) +
  theme_minimal(base_size = 13)

## RELATIVIZADO A CANTIDAD DE PLANTULAS
REL_DMR_Linea <- lmer(DMR ~ Línea + (1|Localidad) + (1|Año), data = relativos)
summary(REL_DMR_Linea) 
anova(REL_DMR_Linea)  #Si
Remm_lineas <- emmeans(REL_DMR_Linea, ~ Línea) #contraste
pairs(Remm_lineas, adjust = "tukey") #J7-L37 y J7-UF93 de peso se mantienen las dif. la k14-L37 en este caso es MARGINAL en el caso anterior no
#grafiquito
Remm_lineas <- emmeans(REL_DMR_Linea, ~ Línea) # Medias estimadas de DMR por Línea
Rcld_lineas <- multcomp::cld(Remm_lineas, Letters = letters, adjust = "tukey")
Rdf_plot <- as.data.frame(Rcld_lineas)

ggplot(Rdf_plot, aes(x = Línea, y = emmean, fill = Línea)) + geom_col(color = "black") + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                                                                                                      width = 0.2, size = 0.8) +
  geom_text(aes(label = .group, y = emmean + 5),   # ajustá el +5 según escala de DMR
            size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(
    x = "Línea",
    y = "Densidad de inflorescencias relativizado a la cantidad de plantulas",
    title = "Medias estimadas de DMR"
  ) +
  theme_minimal(base_size = 13) 


#cuantas semillas produce#



#semillas llenas#