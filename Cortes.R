## Linea temporal
# Pivotar las columnas de días a PF a formato largo
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)  # para p-valores
library(nlme

cols <- paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]

medidas_repetidas <- read_excel(
  path = "CJB_Datos concurso jovenes de bioestadistica.xlsx",
  sheet = "Medidas repetidas en el tiempo"
)
medidas_repetidas$Bloque<- as.factor(medidas_repetidas$Bloque)
medidas_repetidas$Línea<- as.factor(medidas_repetidas$Línea)
medidas_repetidas$Localidad<- as.factor(medidas_repetidas$Localidad)
medidas_repetidas$Corte<- as.factor(medidas_repetidas$Corte)
summary(medidas_repetidas) 
str(medidas_repetidas)

library(dplyr)



summary(relativos$`TC.d 1`)
summary(relativos$`TC.d 2`)
summary(relativos$`TC.d 3`)
summary(relativos$`TC.d 4`)

#Grafico
ggplot(medidas_repetidas,
       aes(x = tiempo, y = PF.d, color = Localidad, group = interaction(Localidad, Línea, Bloque))) +
  stat_summary(aes(group = Localidad),
               fun = mean, geom = "line", size = 1.5) + # línea de la media por localidad
  labs(
    x = "Tiempo (días)",
    y = "Producción por día (PF.d)",
    color = "Localidad"
  ) +
  theme_minimal(base_size = 14)

resumen <- medidas_repetidas %>%
  group_by(Localidad, Línea, tiempo) %>%
  summarise(
    media = mean(PF, na.rm = TRUE),
    sd = sd(PF, na.rm = TRUE),
    n = n()
  ) %>%
  mutate(
    se = sd / sqrt(n),
    IC_inf = media - qt(0.975, df = n-1) * se,
    IC_sup = media + qt(0.975, df = n-1) * se
  )

ggplot(resumen, aes(x = tiempo, y = media, color = Línea, group = Línea)) +
  geom_line() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = IC_inf, ymax = IC_sup), width = 0.2) +
  facet_wrap(~ Localidad) +
  theme_minimal()

#### Modelo ###
library(lme4)
library(lmerTest)  # para p-valores
library(nlme)

# Supongamos que PF.d es la variable respuesta,
# Localidad es efecto fijo,
# Línea y Bloque son efectos aleatorios (anidados)
# Corte sería la medida repetida (tiempo)
M_PDias <- glmer((PF.d/PL_m) ~ Localidad*Línea*tiempo + (1|Bloque),family=gaussian , data = medidas_repetidas)

summary(M_PDias)
anova(M_PDias)

res <- simulateResiduals(M_PDias, n = 1000)
plot(res)
testResiduals(res)
testDispersion(res)
# No se cumplen los supuestos modelo varinza
# Modelo con varianza diferente por localidad

medidas_repetidas_clean <- na.omit(medidas_repetidas[, c("PF.d", "PL_m", "Localidad", "Línea", "tiempo", "Bloque")])
M_PDias <- lme(
  fixed = (PF.d/PL_m) ~ Localidad * Línea * tiempo,
  random = ~1 | Bloque,
  weights = varIdent(form = ~1 | Localidad),
  data = medidas_repetidas_clean
)

#Chequeo supuestos 
plot(M_PDias)     # residuos vs ajustados
qqnorm(resid(M_PDias))
qqline(resid(M_PDias))
library(performance)
check_model(M_PDias) # mejora bastante obvio el VIF es enorme pero no importa es por la interaccion, Tambien da sospecha de que quiza el efecto del tiempo hay transformarlo no sirve la lineal.

medidas_repetidas$Bloque
summary(M_PDias )
anova(M_PDias)




# ### Analizo serie temporal Produccion por dia y hago modelo
# names(relativos)
# str(relativos)
# relativos <- relativos %>%
#   rename(Pl_m = `Pl/m`)
# D_Cortes <- relativos %>%
#   dplyr::select(
#     Año, Localidad, Línea, Bloque, Pl_m,
#     dplyr::starts_with("PF"),
#     dplyr::starts_with("TC")
#   )
# 
# # 1) Pasar a formato largo todas las variables que dependen de corte
# DLargos_Cortes <- D_Cortes %>%
#   pivot_longer(
#     cols = matches("^TC\\.d [1-4]$"),   # Busca TC.d 1, TC.d 2, etc.
#     names_to = "Corte",
#     names_pattern = "TC\\.d (\\d+)",     # Extrae el número del corte
#     values_to = "TCd"
#   ) %>%
#   mutate(
#     Corte = as.integer(Corte),
#     # 2) Definir índice de corte continuo
#     Corte_cont = case_when(
#       Año == "22-23" ~ Corte,         # primer año → cortes 1-3
#       Año == "23-24" ~ Corte + 3,     # segundo año → cortes 4-6
#       TRUE ~ Corte                    # por si aparece otro valor
#       ))
# 
# # Seleccionar sólo columnas de TC.d y pasar a largo
# DLargos_TCd <- relativos %>%
#   pivot_longer(
#     cols = matches("^TC\\.d [1-4]$"),   # Busca TC.d 1, TC.d 2, etc.
#     names_to = "Corte",
#     names_pattern = "TC\\.d (\\d+)",     # Extrae el número del corte
#     values_to = "TCd"
#   ) %>%
#   mutate(
#     Corte = as.integer(Corte),
#     # Definir índice de corte continuo
#     Corte_cont = ifelse(Año == "23-24", Corte, Corte + 3)
#   )
# 
# 
# DLargos_Cortes %>%
#   ggplot(aes(x = Corte_cont, y = TCd, color = Línea, group = Línea)) +
#   stat_summary(fun = mean, geom = "line", size = 1.2) +
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
#   stat_summary(fun = mean, geom = "point", size = 2) +
#   facet_wrap(~ Localidad) +
#   labs(x = "Corte", y = "TC.d (promedio ± error estándar)",
#        title = "Evolución de TC.d por localidad",
#        color = "Línea") +
#   theme_minimal(base_size = 14)
# 
# 
# ###### MODELO #########
# library(lme4)
# DLargos_TCd$Corte_cont<- as.factor(DLargos_TCd$Corte_cont)
# 
# 
# 
# 
# 
# # # Modelo de medidas repetidas
# # M_TCd <- lmer(
# #   TCd ~ Localidad*Corte_cont +  (1|Línea) + (1 | Bloque),
# #   data = DLargos_TCd
# # )
# # 
# # Anova(M_TCd) # HAy interaccion genotipo ambiente en cuanto a la dinamica de produccion por dia
# # summary(M_TCd) #Falta chequer supuestos
# # 
# # # Emmeans para Localidad
# # emm_localidad <- emmeans(M_TCd, ~ Localidad)
# # 
# # # Contrastes por pares (comparaciones múltiples)
# # pairs(emm_localidad)
# # 
# # emm_localidad <- emmeans(M_TCd, ~ Localidad)
# # df_plot <- as.data.frame(emm_localidad)
# # 
# # # # Gráfico estilo Tukey con IC95%
# # # ggplot(df_plot, aes(x = Localidad, y = emmean)) +
# # #   geom_point(size = 3) +
# # #   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
# # #   ylab("TCd (media ajustada)") +
# # #   xlab("Localidad") +
# # #   theme_minimal(base_size = 14)
# # 
# # emm_loc_corte <- emmeans(M_TCd, ~ Localidad | Corte_cont)
# # df_plot <- as.data.frame(emm_loc_corte)
# # 
# # ggplot(df_plot, aes(x = Corte_cont, y = emmean, color = Localidad, group = Localidad)) +
# #   geom_line(size = 1.2) +
# #   geom_point(size = 2) +
# #   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
# #   labs(
# #     x = "Corte",
# #     y = "TCd (media ajustada)",
# #     color = "Localidad"
# #   ) +
# #   theme_minimal(base_size = 14)

###### Hago lo q nos dijo Adriana ####
crudos$`PF Total`
M_PFTotal <- lmer(`PF Total`~ Localidad*Línea + (1|Bloque) + Año , data = relativos)

#supestos
res <- simulateResiduals(M_PFTotal, n = 1000)
plot(res)
testResiduals(res)
testDispersion(res)

summary(M_PFTotal )
anova(M_PFTotal )


# Medias estimadas para Localidad
emm_loc <- emmeans(M_PFTotal, ~ Localidad)
emm_loc

# Comparaciones post hoc con letras
pairs(emmeans(M_PFTotal, ~Línea|Localidad), adjust = "tukey")
cld_loc <- multcomp::cld(emm_loc, Letters = letters, adjust = "tukey")
df_plot_loc <- as.data.frame(cld_loc)

# Paleta de colores
cols <- paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]   # opcional, para personalizar como en tu ejemplo

# Gráfico para Localidad
ggplot(df_plot_loc, aes(x = Localidad, y = emmean, fill = Localidad)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = .group, y = emmean + 30),   # ajustá el +30 según escala de PF Total
            size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(
    x = "Localidad",
    y = "PF Total (media marginal)",
    title = "Medias estimadas de PF Total por localidad"
  ) +
  theme_minimal(base_size = 14)


# ---- Si querés hacer lo mismo por LÍNEA ----
emm_lin <- emmeans(M_PFTotal, ~ Línea)
cld_lin <- multcomp::cld(emm_lin, Letters = letters, adjust = "tukey")
df_plot_lin <- as.data.frame(cld_lin)

ggplot(df_plot_lin, aes(x = Línea, y = emmean, fill = Línea)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, size = 0.8) +
  geom_text(aes(label = .group, y = emmean + 30),   # ajustá el +30 según escala de PF Total
            size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(
    x = "Línea",
    y = "PF Total (media marginal)",
    title = "Medias estimadas de PF Total por línea"
  ) +
  theme_minimal(base_size = 14)

######## Linea*Localidad ###

# Estimación de medias marginales
emm <- emmeans(M_PFTotal, ~ Localidad*Línea)

# Comparaciones múltiples de Tukey
pairs_emm <- contrast(emm, method = "pairwise", adjust = "tukey")
pairs_emm

# Grupos de Tukey (letras)
cld_emm <- multcomp::cld(emm, adjust = "tukey", Letters = letters, alpha = 0.05)
cld_emm

# Gráfico
ggplot(cld_emm, aes(x =interaction(Localidad, Línea), 
                    y = emmean, 
                    fill = Línea)) +
  geom_col(position = position_dodge(width = 0.9), color = "black") +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_text(aes(label = .group, y = emmean + SE + 5), 
            vjust = 0, size = 5) +
  scale_fill_manual(values = cols_mod) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Localidad × Línea", y = "PF Total (media ± SE)",
       title = "Comparaciones entre combinaciones de Localidad y Línea") +
  theme_bw()+
  coord_flip()


