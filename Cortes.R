### Analizo serie temporal Produccion por dia y hago modelo
names(relativos)
str(relativos)
relativos <- relativos %>%
  rename(Pl_m = `Pl/m`)
D_Cortes <- relativos %>%
  dplyr::select(
    Año, Localidad, Línea, Bloque, Pl_m,
    dplyr::starts_with("PF"),
    dplyr::starts_with("TC")
  )

# 1) Pasar a formato largo todas las variables que dependen de corte
DLargos_Cortes <- D_Cortes %>%
  pivot_longer(
    cols = matches("^TC\\.d [1-4]$"),   # Busca TC.d 1, TC.d 2, etc.
    names_to = "Corte",
    names_pattern = "TC\\.d (\\d+)",     # Extrae el número del corte
    values_to = "TCd"
  ) %>%
  mutate(
    Corte = as.integer(Corte),
    # 2) Definir índice de corte continuo
    Corte_cont = case_when(
      Año == "22-23" ~ Corte,         # primer año → cortes 1-3
      Año == "23-24" ~ Corte + 3,     # segundo año → cortes 4-6
      TRUE ~ Corte                    # por si aparece otro valor
      ))

# Seleccionar sólo columnas de TC.d y pasar a largo
DLargos_TCd <- relativos %>%
  pivot_longer(
    cols = matches("^TC\\.d [1-4]$"),   # Busca TC.d 1, TC.d 2, etc.
    names_to = "Corte",
    names_pattern = "TC\\.d (\\d+)",     # Extrae el número del corte
    values_to = "TCd"
  ) %>%
  mutate(
    Corte = as.integer(Corte),
    # Definir índice de corte continuo
    Corte_cont = ifelse(Año == "23-24", Corte, Corte + 3)
  )


###### MODELO #########
library(lme4)

# Modelo de medidas repetidas
M_TCd <- lmer(
  TCd ~ Localidad * Línea + (1 | Bloque) + (1 | Corte_cont),
  data = DLargos_TCd
)

Anova(M_TCd) # HAy interaccion genotipo ambiente en cuanto a la dinamica de produccion por dia
summary(M_TCd) #Falta chequer supuestos

DLargos_Cortes %>%
  ggplot(aes(x = Corte_cont, y = TCd, color = Línea, group = Línea)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~ Localidad) +
  labs(x = "Corte", y = "TC.d (promedio ± error estándar)",
       title = "Evolución de TC.d por localidad",
       color = "Línea") +
  theme_minimal(base_size = 14)


