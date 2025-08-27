## Linea temporal
# Pivotar las columnas de días a PF a formato largo
library(dplyr)
library(tidyr)
library(ggplot2)

medidas_repetidas <- read_excel(
  path = "CJB_Datos concurso jovenes de bioestadistica.xlsx",
  sheet = "Medidas repetidas en el tiempo"
)
summary(medidas_repetidas) 
str(medidas_repetidas)

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

# Supongamos que PF.d es la variable respuesta,
# Localidad es efecto fijo,
# Línea y Bloque son efectos aleatorios (anidados)
# Corte sería la medida repetida (tiempo)

modelo <- lmer(PF.d ~ Localidad*tiempo + (1|Bloque/Línea), data = medidas_repetidas)

summary(modelo)
anova(modelo)





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



