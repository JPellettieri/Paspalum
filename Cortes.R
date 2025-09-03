
# Pivotar las columnas de días a PF a formato largo
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)  # para p-valores
library(nlme)

# cols <- paletteer_d("ggthemes::excel_Depth")
# cols_mod <- cols
# cols_mod[2] <- cols[6]

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

medidas_repetidas_clean <- na.omit(medidas_repetidas[, c("PF.d","PF.ºC","PF.mm", "PL_m", "Localidad", "Línea", "tiempo", "Bloque")])
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
anova(M_PDias)

#EL TIEMPO NO TIENE UNA RELACION LINEAL OBVIAMENTE-> planteo spline natural
library(glmmTMB)
library(splines)
# M_sinInt2 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 2) + (1|Bloque),
#                       family = gaussian , data = medidas_repetidas_clean )
M_sinInt3 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
                      family = gaussian , data = medidas_repetidas_clean )
# M_sinInt4 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 4) + (1|Bloque),
#                     family = gaussian , data = medidas_repetidas_clean )

#M_conInt <- glmmTMB((PF.d/PL_m) ~ Localidad * Línea * ns(tiempo, 3) + (1|Bloque),
#                    family = gaussian, data = medidas_repetidas_clean) #demasiados parámetros para la info disponible. no converge
#anova(M_sinInt, M_conInt)  # LRT

# anova(M_sinInt2 , M_sinInt3 , M_sinInt4) # spline 4 complejiza y no agrega nada al analisis, 3 es significativamnete mejor que 2 
# car::Anova(M_sinInt3)

### Supuestos
res <- simulateResiduals(fittedModel = M_sinInt3, n = 1000)
plot(res)
# Modelo varianza
M_sinInt3_varident <- glmmTMB(
  (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
  dispformula = ~ Localidad,
  family = gaussian,
  data = medidas_repetidas_clean
)
res <- simulateResiduals(fittedModel = M_sinInt3_varident, n = 1000)
#no alcanza cambio a distribucion gamma
M_gamma3 <- glmmTMB(
  (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
  family = Gamma(link = "log"),
  data = medidas_repetidas_clean
)

M_sinInt2 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 2) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_sinInt3 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_conInt3 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea* ns(tiempo, 3) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_sinInt4 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea + ns(tiempo, 4) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_conInt4 <- glmmTMB( (PF.d/PL_m) ~ Localidad * Línea* ns(tiempo, 4) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
anova(M_sinInt2 , M_sinInt3 , M_sinInt4)
### Supuestos #nos quedamos con conInt4 para que haya cohesion entre modelos
res <- simulateResiduals(fittedModel = M_conInt4, n = 1000) # cumple los supuestos
plot(res)

emm_loc_lin <- emmeans(M_conInt4, ~ Línea |Localidad)

emm_loc_lin_resp <- summary(emm_loc_lin, type = "response")
emm_loc_lin_resp

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all)

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all, infer = TRUE)
library(multcomp)
cld_link <- cld(emm_loc_lin, Letters = letters, type = "link")
cld_link

cld_link_df <- as.data.frame(cld_link)

# Convert emm_resp to a plain data.frame
emm_resp_df <- as.data.frame(emm_resp)

# Now join by the factor columns
cld_resp <- left_join(emm_resp_df,
                      cld_link_df[, c("Localidad", "Línea", ".group")],
                      by = c("Localidad", "Línea"))

cld_resp

ggplot(cld_resp, aes(x = Localidad, y = response, color = Línea)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_color_manual(values = cols_mod) + 
  labs(y = "Estimated mean PF.d / PL_m",
       x = "Localidad",
       title = "Estimated marginal means (Gamma GLMM) with group letters") +
  theme_minimal(base_size = 14)



emm_loc_lin <- emmeans(M_conInt4, ~ Localidad |Línea)

emm_loc_lin_resp <- summary(emm_loc_lin, type = "response")
emm_loc_lin_resp

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all)

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all, infer = TRUE)
library(multcomp)
cld_link <- cld(emm_loc_lin, Letters = letters, type = "link")
cld_link

cld_link_df <- as.data.frame(cld_link)

# Convert emm_resp to a plain data.frame
emm_resp_df <- as.data.frame(emm_resp)

# Now join by the factor columns
cld_resp <- left_join(emm_resp_df,
                      cld_link_df[, c("Localidad", "Línea", ".group")],
                      by = c("Localidad", "Línea"))

cld_resp

ggplot(cld_resp, aes(x = Línea, y = response, color = Localidad)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_color_manual(values = cols_mod) + 
  labs(y = "Estimated mean PF.d / PL_m",
       x = "Línea",
       title = "Estimated marginal means (Gamma GLMM) with group letters") +
  theme_minimal(base_size = 14)
###Dinámica temporal de medias marginales
library(ggeffects)
#POR LÍNEA 
pred <- ggpredict(M_conInt4, terms = c("tiempo [all]", "Línea"))


ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Tiempo", y = "Producción relativa (predicha) por día",
       color = " Línea", fill = " Línea") +
  theme_minimal(base_size = 14)
#POR Localidad
pred <- ggpredict(M_conInt4, terms = c("tiempo [all]", "Localidad"))


ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Tiempo", y = "Producción relativa (predicha) por día",
       color = " Localidad", fill = " Localidad") +
  theme_minimal(base_size = 14)

#USAR ESTO COMO TEMPLATE PARA EL DE MEDIAS MARGINALES
#Medias marginales
emm_loc <- emmeans(M_conInt4, ~ Localidad)
emm_lin <- emmeans(M_conInt4, ~ Línea)
pairs_emm <- contrast(emm_loc, method = "pairwise", adjust = "sidak")
pairs_emm
cld_loc <- cld(emm_loc, Letters = letters)
df_loc <- as.data.frame(cld_loc)

# Letras para Línea
cld_lin <- cld(emm_lin, Letters = letters)
cld_lin
df_lin <- as.data.frame(cld_lin)
str(cld_lin)

## 4. Graficar Localidad
ggplot(df_loc, aes(x = Localidad, y = exp(emmean))) +   # exp() porque el link es log
  geom_col(fill = cols_mod[1], color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)),
                width = 0.2) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  labs(y = "Estimated mean PF.d / PL_", x = "Localidad",
       title = "Medias marginales estimadas por Localidad") +
  theme_bw()

## 5. Graficar Línea
ggplot(df_lin, aes(x = Línea, y = exp(emmean), fill = Línea)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)),
                width = 0.2) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_fill_manual(values = cols_mod) +
  labs(y = "Estimated mean PF.d / PL_", x = "Línea",
       title = "Medias marginales estimadas por Línea") +
  theme_bw()

#POR GRADOS####
summary(medidas_repetidas)
M_grados <-  glmmTMB(
  (PF.ºC/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
  family = Gamma(link = "log"),
  data = medidas_repetidas_clean
)

M_sinInt2 <- glmmTMB( (PF.ºC/PL_m) ~ Localidad * Línea + ns(tiempo, 2) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_sinInt3 <- glmmTMB( (PF.ºC/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_conInt3 <- glmmTMB( (PF.ºC/PL_m) ~ Localidad * Línea* ns(tiempo, 3) + (1|Bloque),
                     family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_sinInt4 <- glmmTMB( (PF.ºC/PL_m) ~ Localidad * Línea + ns(tiempo, 4) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_conInt4 <- glmmTMB( (PF.ºC/PL_m) ~ Localidad * Línea* ns(tiempo, 4) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
anova(M_sinInt2 , M_sinInt3 , M_sinInt4) #Me quedo con el modelo conInt4
### Supuestos
res <- simulateResiduals(fittedModel = M_conInt4, n = 1000) # cumple los supuestos
plot(res)

emm_loc_lin <- emmeans(M_conInt4, ~ Línea |Localidad)

emm_loc_lin_resp <- summary(emm_loc_lin, type = "response")
emm_loc_lin_resp

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all)

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all, infer = TRUE)
library(multcomp)
cld_link <- cld(emm_loc_lin, Letters = letters, type = "link")
cld_link

cld_link_df <- as.data.frame(cld_link)

# Convert emm_resp to a plain data.frame
emm_resp_df <- as.data.frame(emm_resp)

# Now join by the factor columns
cld_resp <- left_join(emm_resp_df,
                      cld_link_df[, c("Localidad", "Línea", ".group")],
                      by = c("Localidad", "Línea"))

cld_resp

ggplot(cld_resp, aes(x = Localidad, y = response, color = Línea)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_color_manual(values = cols_mod) + 
  labs(y = "Estimated mean PFºC / PL_m",
       x = "Localidad",
       title = "Estimated marginal means (Gamma GLMM) with group letters") +
  theme_minimal(base_size = 14)



emm_loc_lin <- emmeans(M_conInt4, ~ Localidad |Línea)

emm_loc_lin_resp <- summary(emm_loc_lin, type = "response")
emm_loc_lin_resp

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all)

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all, infer = TRUE)
library(multcomp)
cld_link <- cld(emm_loc_lin, Letters = letters, type = "link")
cld_link

cld_link_df <- as.data.frame(cld_link)

# Convert emm_resp to a plain data.frame
emm_resp_df <- as.data.frame(emm_resp)

# Now join by the factor columns
cld_resp <- left_join(emm_resp_df,
                      cld_link_df[, c("Localidad", "Línea", ".group")],
                      by = c("Localidad", "Línea"))

cld_resp

ggplot(cld_resp, aes(x = Línea, y = response, color = Localidad)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_color_manual(values = cols_mod) + 
  labs(y = "Estimated mean PFºC / PL_m",
       x = "Línea",
       title = "Estimated marginal means (Gamma GLMM) with group letters") +
  theme_minimal(base_size = 14)
###Dinámica temporal de medias marginales
library(ggeffects)
#POR LÍNEA 
pred <- ggpredict(M_conInt4, terms = c("tiempo [all]", "Línea"))


ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Tiempo", y = "Producción relativa (predicha) por grado",
       color = " Línea", fill = " Línea") +
  theme_minimal(base_size = 14)
#POR Localidad
pred <- ggpredict(M_conInt4, terms = c("tiempo [all]", "Localidad"))


ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Tiempo", y = "Producción relativa (predicha) por grado",
       color = " Localidad", fill = " Localidad") +
  theme_minimal(base_size = 14)

#USAR ESTO COMO TEMPLATE PARA EL DE MEDIAS MARGINALES
#Medias marginales
emm_loc <- emmeans(M_conInt4, ~ Localidad)
emm_lin <- emmeans(M_conInt4, ~ Línea)
cld_loc <- cld(emm_loc, Letters = letters)
df_loc <- as.data.frame(cld_loc)

# Letras para Línea
cld_lin <- cld(emm_lin, Letters = letters)
df_lin <- as.data.frame(cld_lin)
str(cld_lin)

## 4. Graficar Localidad
ggplot(df_loc, aes(x = Localidad, y = exp(emmean))) +
  geom_col(fill = cols_mod[1], color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL),
                width = 0.2)) +
  geom_text(aes(label = .group), vjust = -0.8, size = 5) +   # letras arriba
  labs(y = "Estimated mean PFºC / PL_", x = "Localidad",
       title = "Medias marginales estimadas por Localidad") +
  theme_bw()
## 5. Graficar Línea
ggplot(df_lin, aes(x = Línea, y = exp(emmean), fill = Línea)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)),
                width = 0.2) +
  scale_fill_manual(values = cols_mod) +
  geom_text(aes(label = .group), vjust = -0.8, size = 5) +
  labs(y = "Estimated mean PFºC / PL_", x = "Línea",
       title = "Medias marginales estimadas por Línea") +
  theme_bw()

####POR MM####

M_sinInt2 <- glmmTMB( (PF.mm/PL_m) ~ Localidad * Línea + ns(tiempo, 2) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_sinInt3 <- glmmTMB( (PF.mm/PL_m) ~ Localidad * Línea + ns(tiempo, 3) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_conInt3 <- glmmTMB( (PF.mm/PL_m) ~ Localidad * Línea* ns(tiempo, 3) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_sinInt4 <- glmmTMB( (PF.mm/PL_m) ~ Localidad * Línea + ns(tiempo, 4) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
M_conInt4 <- glmmTMB( (PF.mm/PL_m) ~ Localidad * Línea* ns(tiempo, 4) + (1|Bloque),
                      family = Gamma(link = "log"), , data = medidas_repetidas_clean )
anova(M_sinInt2 , M_sinInt3 , M_sinInt4) #de nuevo con el sinInt4 MENTIRA NO DAN LOS SUPUESTOS
anova(M_sinInt4, M_conInt4)
res <- simulateResiduals(fittedModel = M_conInt4, n = 1000) # cumple los supuestos el conINT4
plot(res)

emm_loc_lin <- emmeans(M_conInt4, ~ Línea |Localidad)

emm_loc_lin_resp <- summary(emm_loc_lin, type = "response")
emm_loc_lin_resp

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all)

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all, infer = TRUE)
library(multcomp)
cld_link <- cld(emm_loc_lin, Letters = letters, type = "link")
cld_link

cld_link_df <- as.data.frame(cld_link)

# Convert emm_resp to a plain data.frame
emm_resp_df <- as.data.frame(emm_resp)

# Now join by the factor columns
cld_resp <- left_join(emm_resp_df,
                      cld_link_df[, c("Localidad", "Línea", ".group")],
                      by = c("Localidad", "Línea"))

cld_resp

ggplot(cld_resp, aes(x = Localidad, y = response, color = Línea)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_color_manual(values = cols_mod) + 
  labs(y = "Estimated mean PFºC / PL_m",
       x = "Localidad",
       title = "Estimated marginal means (Gamma GLMM) with group letters") +
  theme_minimal(base_size = 14)



emm_loc_lin <- emmeans(M_conInt4, ~ Localidad |Línea)

emm_loc_lin_resp <- summary(emm_loc_lin, type = "response")
emm_loc_lin_resp

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all)

pairs_all <- pairs(emm_loc_lin)
summary(pairs_all, infer = TRUE)
library(multcomp)
cld_link <- cld(emm_loc_lin, Letters = letters, type = "link")
cld_link

cld_link_df <- as.data.frame(cld_link)

# Convert emm_resp to a plain data.frame
emm_resp_df <- as.data.frame(emm_resp)

# Now join by the factor columns
cld_resp <- left_join(emm_resp_df,
                      cld_link_df[, c("Localidad", "Línea", ".group")],
                      by = c("Localidad", "Línea"))

cld_resp

ggplot(cld_resp, aes(x = Línea, y = response, color = Localidad)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_text(aes(label = .group),
            position = position_dodge(width = 0.6),
            vjust = -0.8, size = 5) +
  scale_color_manual(values = cols_mod) + 
  labs(y = "Estimated mean PFºC / PL_m",
       x = "Línea",
       title = "Estimated marginal means (Gamma GLMM) with group letters") +
  theme_minimal(base_size = 14)
###Dinámica temporal de medias marginales
library(ggeffects)
#POR LÍNEA 
pred <- ggpredict(M_conInt4, terms = c("tiempo [all]", "Línea"))


ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Tiempo", y = "Producción relativa (predicha) por mm",
       color = " Línea", fill = " Línea") +
  theme_minimal(base_size = 14)
#POR Localidad
pred <- ggpredict(M_conInt4, terms = c("tiempo [all]", "Localidad"))


ggplot(pred, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(x = "Tiempo", y = "Producción relativa (predicha) por mm",
       color = " Localidad", fill = " Localidad") +
  theme_minimal(base_size = 14)

#USAR ESTO COMO TEMPLATE PARA EL DE MEDIAS MARGINALES
#Medias marginales
emm_loc <- emmeans(M_conInt4, ~ Localidad)
emm_lin <- emmeans(M_conInt4, ~ Línea)
cld_loc <- cld(emm_loc, Letters = letters)
df_loc <- as.data.frame(cld_loc)
emm_CC <- emmeans(M_conInt4, ~ Línea| Localidad)
pairs_emm <- contrast(emm_CC, method = "pairwise", adjust = "tukey")
pairs_emm
cld_CC <- cld(emm_CC, Letters = letters)
cld_CC
df_CC <- as.data.frame(cld_CC)
# Letras para Línea
cld_lin <- cld(emm_lin, Letters = letters)
df_lin <- as.data.frame(cld_lin)
str(cld_lin)

## 4. Graficar Localidad
ggplot(df_loc, aes(x = Localidad, y = exp(emmean))) +
  geom_col(fill = cols_mod[1], color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL),
                    width = 0.2)) +
  geom_text(aes(label = .group), vjust = -0.8, size = 5) +   # letras arriba
  labs(y = "Estimated mean PFmm / PL_", x = "Localidad",
       title = "Medias marginales estimadas por Localidad") +
  theme_bw()
## 5. Graficar Línea
ggplot(df_lin, aes(x = Línea, y = exp(emmean), fill = Línea)) +
  geom_col(color = "black") +
  geom_errorbar(aes(ymin = exp(asymp.LCL), ymax = exp(asymp.UCL)),
                width = 0.2) +
  scale_fill_manual(values = cols_mod) +
  geom_text(aes(label = .group), vjust = -0.8, size = 5) +
  labs(y = "Estimated mean PFmm / PL_", x = "Línea",
       title = "Medias marginales estimadas por Línea") +
  theme_bw()

###################################################################
##########################    Producion Total      ###########################################
#####################################################################



###### Hago lo q nos dijo Adriana ####
crudos$`PF Total`
M_PFTotal <- lmer(`PF Total`~ Localidad*Línea + (1|Bloque) + Año , data = relativos)
InteracM_PFTotal <- lmer(`PF Total`~ Localidad*Línea*Año + (1|Bloque) , data = relativos)

anova(M_PFTotal,InteracM_PFTotal)
#supestos
res <- simulateResiduals(M_PFTotal, n = 1000)
plot(res)
testResiduals(res)
testDispersion(res)
summary(M_PFTotal )
anova(M_PFTotal )

#NO ES HOMOGENEA LA VARIANZA
library(nlme)
relativos <- relativos %>%
  dplyr::rename(PF_Total = `PF Total`)
M_PFTotal_varIdent <- lme(
  PF_Total ~ Localidad * Línea + Año,
  random = ~ 1 | Bloque,
  weights = varIdent(form = ~ 1 | Localidad*Línea),
  data = relativos
)
#CHEQUEO QUE LA NORMALIDAD SIGA BIEN
res <- residuals(M_PFTotal_varIdent, type = "normalized")

# Shapiro-Wilk test
shapiro.test(res)

# QQplot
qqnorm(res)
qqline(res, col = "red", lwd = 2)
#MURIÓ LA NORMALIDAD!!! VAMOS A TENER QUE PROBAR CON OTRO MODELO
# Pruebo con Gamma
M_gamma <- glmer(PF_Total ~ Localidad * Línea + Año + (1|Bloque),
                 family = Gamma(link = "log"),
                 data = relativos)

res <- simulateResiduals(M_gamma, n = 1000)
plot(res)
testResiduals(res)
testDispersion(res)

summary(M_gamma)
car::Anova(M_gamma, type =2)


# Medias estimadas para Localidad
#LO CAMBIË PARA QUE USE EL MODELO GAMMA
emm_loc <- emmeans(M_gamma, ~ Localidad |Línea)
emm_loc

# Comparaciones post hoc con letras
pairs(emmeans(M_gamma, ~Localidad|Línea), adjust = "sidak")
cld_loc <- multcomp::cld(emm_loc, Letters = letters, adjust = "sidak")

# Paleta de colores
cols <- paletteer::paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]   # opcional, para personalizar como en tu ejemplo

emm_df <- as.data.frame(emm_loc)
cld_df <- data.frame(
  Localidad = cld_loc$Localidad,
  Línea = cld_loc$Línea,
  label = cld_loc$.group
)
cld_df
emm_df
# Gráfico para Localidad
ggplot(emm_df, aes(x = Localidad, y = emmean, fill = Línea)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(data = cld_df,
            aes(x = Localidad, y = max(emm_df$emmean) + 0.05, label = label),
            inherit.aes = FALSE,
            position = position_dodge(0.9)) +
  facet_wrap(~Línea) +
  scale_fill_manual(values = cols_mod) +
  theme_minimal(base_size = 14) +
  labs(
    y = "PF Total (estimado)",
    x = "Localidad",
    fill = "Línea",
    title = "Efectos marginales de Localidad por Línea",
    subtitle = "Letras indican diferencias significativas (Sidak)"
  )
# ggplot(df_plot_loc, aes(x = Localidad, y = emmean)) +
#   geom_col(color = "black", fill = "darkgreen") +
#   geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
#                 width = 0.2, size = 0.8) +
#   geom_text(aes(label = .group, y = emmean + 3),   # ajustá el +30 según escala de PF Total
#             size = 5)  +
#   labs(
#     x = "Localidad",
#     y = "PF Total (media marginal)",
#     title = "Medias estimadas de PF Total por localidad"
#   ) +
#   theme_minimal(base_size = 14)


# ---- Si querés hacer lo mismo por LÍNEA ----
# Medias estimadas para Localidad
#LO CAMBIË PARA QUE USE EL MODELO GAMMA
emm_lin <- emmeans(M_gamma, ~ Línea |Localidad)
emm_lin

# Comparaciones post hoc con letras
pairs(emmeans(M_gamma, ~ Línea |Localidad), adjust = "sidak")
cld_lin <- multcomp::cld(emm_lin, Letters = letters, adjust = "sidak")

# Paleta de colores
cols <- paletteer::paletteer_d("ggthemes::excel_Depth")
cols_mod <- cols
cols_mod[2] <- cols[6]   # opcional, para personalizar como en tu ejemplo

emm_df <- as.data.frame(emm_lin)
cld_df <- data.frame(
  Localidad = cld_lin$Localidad,
  Línea = cld_lin$Línea,
  label = cld_lin$.group
)
cld_df
emm_df
# Gráfico para Localidad
ggplot(emm_df, aes(x = Línea, y = emmean, fill = Localidad)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(data = cld_df,
            aes(x = Línea, y = max(emm_df$emmean) + 0.05, label = label),
            inherit.aes = FALSE,
            position = position_dodge(0.9)) +
  facet_wrap(~Localidad) +
  scale_fill_manual(values = cols_mod) +
  theme_minimal(base_size = 14) +
  labs(
    y = "PF Total (estimado)",
    x = "Línea",
    fill = "Localidad",
    title = "Efectos marginales de Línea por Localidad",
    subtitle = "Letras indican diferencias significativas (Sidak)"
  )

# emm_lin <- emmeans(M_PFTotal, ~ Línea)
# cld_lin <- multcomp::cld(emm_lin, Letters = letters, adjust = "tukey")
# df_plot_lin <- as.data.frame(cld_lin)
# 
# ggplot(df_plot_lin, aes(x = Línea, y = emmean, fill = Línea)) +
#   geom_col(color = "black") +
#   geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
#                 width = 0.2, size = 0.8) +
#   geom_text(aes(label = .group, y = emmean + 30),   # ajustá el +30 según escala de PF Total
#             size = 5) +
#   scale_fill_manual(values = cols_mod) +
#   labs(
#     x = "Línea",
#     y = "PF Total (media marginal)",
#     title = "Medias estimadas de PF Total por línea"
#   ) +
#   theme_minimal(base_size = 14)
# 
# ######## Linea*Localidad ###

# Estimación de medias marginales
emm <- emmeans(M_gamma, ~ Localidad*Línea)

# ComparacionesM_gamma# Comparaciones múltiples de Tukey
pairs_emm <- contrast(emm, method = "pairwise", adjust = "sidak")
pairs_emm

# Grupos de Tukey (letras)
cld_emm <- multcomp::cld(emm, adjust = "sidak", Letters = letters, alpha = 0.05)
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

