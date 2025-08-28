
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
library(multcompView)
# Paleta de colores 
cols <- paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]

str(relativos)
summary(relativos$DMT)
summary(relativos$DMR)
summary(relativos$`Prod. Sem`)
sd(relativos$`Prod. Sem`)
#  5.	DMR
#"Medio que ya lo hicimos antes, habria q ver de encarar huntas las preguntas y despues ir al detalle y ya",
#  6.	¿Existen diferencias entre las localidades en la densidad de inflorescencias, producción de semillas y porcentaje de llenado de semillas?
#"same shit",
#  7.	Cuál sería el mejor híbrido y cuál sería la localidad más apropiada para la producción de semillas?
#"aca conclu con las dif sig etc no es la combinacion!" # tener en cuenta cuantas de las semillas dieron plantulas?


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
#####                    MODELOS DMR                                    #######
## Hay dif sig entre lineas?
# modelo<- lmer(DMR ~ Línea*Localidad*Año + (1|Bloque), data = relativos, weights = varIdent(form = ~1 | Localidad)) #ziformula porque hay tantos 0 que rompe poisson y nbinom2 porque poisson no se banca el darma
# 
# library(nlme)
# DMR <- lme(DMR ~ Línea*Localidad + Año,
#               random = ~1|Bloque,
#               weights = varIdent(form = ~1 | Localidad),
#               data = relativos,
#               na.action = na.omit) 
#no cumple supuestos, aun con varident nos da sig el outliers

M_DMRInt <- glmmTMB(
  DMR ~ Localidad *Línea*Año,
  family = tweedie(link="log"),
  data = relativos)

M_DMR <- glmmTMB(
  DMR ~ Localidad *Línea+Año,
  family = tweedie(link="log"),
  data = relativos)

anova(M_DMRInt,M_DMR) # las dif no son significativas me quedo con el modelo sin interaccion


#supuestos
res <- simulateResiduals(M_DMR)
plot(res)
testDispersion(res)#  Test de sobredispersión
testZeroInflation(res)#  Test de cero-inflación (exceso de ceros)

#Medias marginales
emm_loc <- emmeans(M_DMR, ~ Localidad)
emm_lin <- emmeans(M_DMR, ~ Línea)
df_loc <- as.data.frame(emm_loc)
df_lin <- as.data.frame(emm_lin)

## 4. Graficar Localidad
ggplot(df_loc, aes(x = Localidad, y = exp(emmean))) +   # exp() porque el link es log
  geom_col(fill = cols_mod[1], color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)),
                width = 0.2) +
  labs(y = "DMR (media marginal)", x = "Localidad",
       title = "Medias marginales estimadas por Localidad") +
  theme_bw()

## 5. Graficar Línea
ggplot(df_lin, aes(x = Línea, y = exp(emmean), fill = Línea)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)),
                width = 0.2) +
  scale_fill_manual(values = cols_mod) +
  labs(y = "DMR (media marginal)", x = "Línea",
       title = "Medias marginales estimadas por Línea") +
  theme_bw()






              # summary(M_DMR) 
              # anova(M_DMR)  #Si
              # emm_lineas <- emmeans(M_DMR, ~ Línea|Localidad+Año) #contraste
              # pairs(emm_lineas, adjust = "tukey") #J7-L37 y J7-UF93
              #  #grafiquito
              # emm_lineas <- emmeans(M_DMR ~ Línea*Localidad+Año) # Medias estimadas de DMR por Línea
              # cld_lineas <- multcomp::cld(emm_lineas, Letters = letters, adjust = "tukey")
              # df_plot <- as.data.frame(cld_lineas)
              # 
              # ggplot(df_plot, aes(x = Línea, y = emmean, fill = Línea)) + geom_col(color = "black") + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
              #                 width = 0.2, size = 0.8) +
              #   geom_text(aes(label = .group, y = emmean + 5),   # ajustá el +5 según escala de DMR
              #             size = 5) +
              #   scale_fill_manual(values = cols_mod) +
              #   labs(
              #     x = "Línea",
              #     y = "Densidad de inflorescencias (DMR, predicho)",
              #     title = "Medias estimadas de DMR por línea"
              #   ) +
              #   theme_minimal(base_size = 13)

## RELATIVIZADO A CANTIDAD DE PLANTULAS ####
REL_DMR_Linea <- lmer(DMR ~ Línea + (1|Localidad) + (1|Año), data = relativos) ##Cambiar poner poison!
summary(REL_DMR_Linea) 
anova(REL_DMR_Linea)  #Si
Remm_lineas <- emmeans(REL_DMR_Linea, ~ Línea) #contraste
pairs(Remm_lineas, adjust = "tukey") #J7-L37 y J7-UF93 de pedo se mantienen las dif. la k14-L37 en este caso es MARGINAL en el caso anterior no
#grafiquito

Remm_lineas <- emmeans(REL_DMR_Linea, ~ Línea) # Medias estimadas de DMR por Línea
#Rcld_lineas <- cld(Remm_lineas, Letters = letters, adjust = "tukey")
Rdf_plot <- as.data.frame(Remm_lineas)

ggplot(Rdf_plot, aes(x = Línea, y = emmean, fill = Línea)) + geom_col(color = "black") + geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                                                                                                      width = 0.2, size = 0.8) +
  #geom_text(aes(label = .group, y = emmean + 5),   # ajustá el +5 según escala de DMR
  #          size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(
    x = "Línea",
    y = "Densidad de inflorescencias relativizado a la cantidad de plantulas",
    title = "Medias estimadas de DMR" ) + theme_minimal(base_size = 13) 








#cuantas semillas produce#



#semillas llenas#

## Hay dif entre localidades? # sin relativizar 
# Modelo con Localidad como VE y Línea/Año como efectos aleatorios
modelo_loc <- glmmTMB(
  DMR ~ Localidad *Línea + Año,
  family = tweedie(link="log"),
  data = relativos)

res <- simulateResiduals(modelo_loc)
plot(res)
testDispersion(res)#  Test de sobredispersión
testZeroInflation(res)#  Test de cero-inflación (exceso de ceros)

summary(modelo_loc)
car::Anova(modelo_loc) # Localidad 43.679  3  1.765e-09 ***
#contraste
tukey_loc <- pairs(emm_loc, adjust = "tukey")
tukey_loc
# Medias estimadas
emm_loc <- emmeans(modelo_loc, ~ Localidad, type = "response") 
emm_loc
# Obtenemos los intervalos de confianza
df_plot_loc <- as.data.frame(emm_loc)
df_plot_loc$.group <- cld_loc$.group  # agregamos las letras

# Paleta de colores
cols <- paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]

# Gráfico
ggplot(df_plot_loc, aes(x = Localidad, y = response , fill = Localidad)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = .group, y = response  + 2), size = 5) +  # ajusté +2 para que las letras queden dentro del eje
  scale_fill_manual(values = cols_mod) +
  scale_y_continuous(limits = c(0, 1)) +  # ajusta según rango real
  labs(
    x = "Localidad",
    y = "Densidad de inflorescencias (DMR, predicho)",
    title = "Medias estimadas de DMR por localidad"
  ) +
  theme_minimal()



#### Interaccion genotipo ambiente #### 
# Modelo con Localidad como VE y Línea/Año como efectos aleatorios
# modelo_GEI <- lmer(DMR ~ Localidad*Línea + (1|Año), data = relativos) # 
# 
# #supuestos
# # Residuos del modelo
# residuos <- residuals(modelo_GEI)
# shapiro.test(residuos) # no me quedan dudas xd  p-value = 2.723e-07

library(glmmTMB)
modelo_GEI <- glmmTMB(
  DMR ~ Localidad*Línea + (1|Año),
  family = tweedie(link="log"),
  data = relativos
)
res <- simulateResiduals(modelo_GEI)
plot(res)
testDispersion(res)#  Test de sobredispersión
testZeroInflation(res)#  Test de cero-inflación (exceso de ceros)


# Medias estimadas de DMR por Localidad
emm_GEI <- emmeans(modelo_GEI, ~ Localidad)



summary(modelo_GEI) 
anova(modelo_GEI)  #Si
emm_lineas <- emmeans(modelo_GEI, ~ Línea) #contraste
pairs(emm_lineas, adjust = "tukey") #J7-L37 y J7-UF93
emm_localidad <- emmeans(modelo_GEI, ~ Localidad)
pairs(emm_localidad, adjust = "tukey")
# Comparaciones post hoc con letras
cld_loc <- multcomp::cld(emm_loc, Letters = letters, adjust = "tukey")
df_plot_loc <- as.data.frame(cld_loc)

# Paleta de colores
cols <- paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]

# Gráfico
ggplot(df_plot_loc, aes(x = Localidad, y = emmean, fill = Localidad)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = .group, y = emmean + 5),   # ajustá el +5 según escala de DMR
            size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(
    x = "Localidad",
    y = "Densidad de inflorescencias (DMR, predicho)",
    title = "Medias estimadas de DMR por localidad"
  ) +
  theme_minimal()

