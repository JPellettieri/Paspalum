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
    cols = matches("^(PF|TC\\.|TTaPF|Ppt a PF|días a PF) [1-4]$"), 
    names_to = c("Variable", "Corte"),
    names_pattern = "^(.*) (\\d+)$",
    values_to = "Valor"
  ) %>%
  mutate(
    Corte = as.integer(Corte),
    # 2) Definir índice de corte continuo
    Corte_cont = case_when(
      Año == "22-23" ~ Corte,         # primer año → cortes 1-3
      Año == "23-24" ~ Corte + 3,     # segundo año → cortes 4-6
      TRUE ~ Corte                    # por si aparece otro valor
      ))
# 3) Ahora tenés todas las variables en formato largo con corte continuo

DLargos_Cortes %>%
  arrange(Localidad, Línea, Bloque, Corte_cont) %>%
  head(20)
