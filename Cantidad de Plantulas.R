
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
library(paletteer)


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
#VR: Cantidad de plantulas (es un conteo-> poisson) !!


M_Plantulas <- glm(`Pl/m` ~ Localidad * Línea,
                   family = poisson(link = "log"),
                   data = crudos)
#Chequeo supuestos:
# 1. Prueba de sobredispersión
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model, type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq / rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq, ratio=prat, rdf=rdf, p=pval)
}
overdisp_fun(M_Plantulas) # TERRIBLE SOBREDISPERSION HAY

library(DHARMa)
simres <- simulateResiduals(M_Plantulas)
plot(simres)   # gráfica diagnóstica
testDispersion(simres)
testZeroInflation(simres)  # NO SE CUMPLE NI UN SUPUESTO :O

####### HAY SOBRE DISPERSION HAGO BINOM NEGATIVA ########
library(MASS)
M_Plantulas_nb <- glm.nb(`Pl/m` ~ Localidad * Línea, data = crudos) #VA año
Anova(M_Plantulas_nb, type = 3)
summary(M_Plantulas_nb)

#Supuestos (esta vez dan bien)
simres_nb <- simulateResiduals(M_Plantulas_nb)
plot(simres_nb)
testDispersion(simres_nb)
Anova(M_Plantulas_nb, type = 3) # todo super significativo

#Contrastes a priori (quiero comparar entre lineas de un mismo lugar y entre lugares de una misma linea no todo contra todo)
emm_lineas <- emmeans(M_Plantulas_nb, ~ Línea*Localidad, type = "response") 
# Comparaciones solo entre localidades para cada línea
contrastes_lugares <- contrast(emm_lineas, 
                               method = "pairwise", 
                               by = "Línea",      # separa por línea
                               adjust = "none")   
contrastes_lugares

# J7: 
#Cerro Azul / Reconquista     1.514 0.295 Inf     1.034     2.217    1   2.129  0.0332
#Colonia Caroya / Reconquista 2.028 0.391 Inf     1.390     2.958    1   3.668  0.0002
# Corrientes / Reconquista     1.849 0.357 Inf     1.266     2.700    1   3.179  0.0015

#K14
#Cerro Azul / Reconquista     2.421 0.464 Inf     1.663     3.524    1   4.616  <.0001
#Colonia Caroya / Corrientes  0.557 0.103 Inf     0.387     0.802    1  -3.149  0.0016
#Colonia Caroya / Reconquista 1.692 0.328 Inf     1.158     2.473    1   2.717  0.0066
#Corrientes / Reconquista     3.036 0.579 Inf     2.090     4.411    1   5.829  <.0001

# L37
# Cerro Azul / Reconquista     1.620 0.304 Inf     1.122     2.339    1   2.576  0.0100
# Corrientes / Reconquista     1.733 0.324 Inf     1.201     2.500    1   2.940  0.0033
# 
# UF93:
# Cerro Azul / Colonia Caroya  1.668 0.307 Inf     1.163     2.392    1   2.782  0.0054
# Cerro Azul / Reconquista     2.627 0.492 Inf     1.820     3.793    1   5.155  <.0001
# Colonia Caroya / Corrientes  0.668 0.123 Inf     0.465     0.958    1  -2.192  0.0284
# Colonia Caroya / Reconquista 1.575 0.299 Inf     1.086     2.284    1   2.394  0.0167
# Corrientes / Reconquista     2.359 0.443 Inf     1.633     3.408    1   4.571  <.0001
#   
  
contrastes_lineas <- contrast(emm_lineas, 
                              method = "pairwise", 
                              by = "Localidad",  # separa por localidad
                              adjust = "none")   

contrastes_lineas 

#  Cerro Azul:
#   contrast   ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# J7 / K14   0.617 0.1160 Inf     0.427     0.891    1  -2.576  0.0100
# J7 / L37   0.604 0.1130 Inf     0.419     0.873    1  -2.686  0.0072
# J7 / UF93  0.438 0.0815 Inf     0.304     0.630    1  -4.439  <.0001
# 
# Localidad = Colonia Caroya:
# 
# Localidad = Corrientes:
#   contrast   ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# J7 / K14   0.601 0.1110 Inf     0.418     0.863    1  -2.754  0.0059
# J7 / UF93  0.595 0.1100 Inf     0.414     0.856    1  -2.803  0.0051
# 
# 
# Localidad = Reconquista:
#   contrast   ratio     SE  df asymp.LCL asymp.UCL null z.ratio p.value
# J7 / L37   0.647 0.1260 Inf     0.442     0.947    1  -2.239  0.0252
# K14 / L37  0.656 0.1270 Inf     0.448     0.960    1  -2.171  0.0300


## GRAFICOS ##


# Emmeans en escala de respuesta
emm_lineas <- emmeans(M_Plantulas_nb, ~ Línea*Localidad, type = "response")
df_plot <- as.data.frame(emm_lineas)

cols <- paletteer_d("ggthemes::excel_Depth")

# Reemplazar 4° por el 5°
cols_mod <- cols
cols_mod[2] <- cols[6]

# Usar en el gráfico
ggplot(df_plot, aes(x = Localidad, y = response, fill = Línea)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(x = "Localidad", y = "Plántulas por metro (predicho)",
       title = "Predicciones del modelo negativo binomial") +
  scale_fill_manual(values = cols_mod) +
  theme_minimal()

