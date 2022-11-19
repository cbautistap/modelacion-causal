# Sesión 08. Propensity score matching

library(haven) # para leer dta
library(stargazer)
library(magrittr)
library(tidyverse)
library(knitr)
library(kableExtra)
library(MatchIt) # cuenta con rutinas para hacer matching

ecls <- read.csv("data/ecls.csv")
attach(ecls)
View(ecls)

# I. Obtener diferencias entre medias para la variable de resultado para el
# grupo de tratamiento y control (punto de partida, sería ATE inicial)

# variable resultado: puntuación de matemáticas estandarizada: "c5r2mtsc_std

# I.1 calcula medias y error estándar
ecls %>% group_by(catholic) %>% # agrupa por privadas (catholic) = 1 y públicas (non catholic) = 0
  summarise(n_students = n(), # cuenta para ambos grupos cuantos hay
            mean_math = mean(c5r2mtsc_std), # media de cada grupo
            std_error = sd(c5r2mtsc_std)/sqrt(n_students)) %>% # desv est MUESTRAL de cada grupo 
  kable()

# Observamos en la tabla:
# a) desbalance (menos van a escuelas privadas)
# b) media es negativa porque está estandarizada, más fácil interpretar
# El puntaje promedio de matemáticas de los estudiantes de escuelas católicas (privadas) es
# más del 20% (.193 - (-0.030)) de una desviación estándar más alta que la de los estudiantes
# de escuelas públicas.
# c) privados tienen más dispersión (casi el doble). Ver std_error. Por lo tanto, media 
# de públicos es más homogénea.

# Por lo tanto, estamos omitiendo muchos factores!

# I.2. Significancia de diferencia en medias
with(ecls, t.test(c5r2mtsc_std ~ catholic)) 
# vemos que la diferencia sí es estadísticamente significativa.
# estadístico t es muy alto (valor absoluto) y por lo tanto, pvalue muy pequeño.
# SE RECHAZA H0: diferencia en medias igual a cero.

# II. Evalúe las diferencias entre medias para COVARIABLES entre grupos de tratamiento y control

# t test nos ayuda a evaluar si hay equilibrio.
# Se eligen 5 covariables: raza blanca, edad de la madre, ingresos, número de lugares donde niño ha vivido (estabilidad) en los últimos 4., nivel educación madre mayor a HS o no. 
ecls_cov <- c('race_white', 'p5hmage', 'w3income', 'p5numpla', 'w3momed_hsb')
# nota. Literatura ha mostrado que es más relevante EDAD de la madre

# II.1 calcula medias para covariables:
ecls %>% group_by(catholic) %>% # mismo procedimiento que en I (solo para medias y sin std error), pero para MÚLTIPLES VARIABLES
  select(one_of(ecls_cov)) %>% 
  summarise_all(funs(mean(., na.rm = T))) %>% 
  kable()

# II.2 se puede comprobar si las diferencias por grupo son significativas mediante la prueba
# de hipótesis t apropiada.
lapply(ecls_cov,
       function(v) {t.test(ecls[, v] ~ ecls[, 'catholic'])}) # Mismo procedimiento que I.2 para múltiples variables

# OJO. Si nuestra medición fuera ideal, o experimento ALEATORIO, esperaríamos que NO haya dif en medias de covariables (no rechazar)
# Los resultados muestran que las diferencias de medias para todas las covariables evaluadas
# son significativas, i.e. rechazan HO: diff_cov_i = 0

# III. Estime propensity score
# como probabilidad de recibir tratamiento dado un conjunto de covariables previas al tratamiento
# estimar modelo en particular que permita calcular prob de ocurrencia de que 
# la obs esté dentro del grupo de tratamiento y no en el de control

# III.1 Usamos un modelo logit. Estima prob de que ocurra dicotómica dados controles
ecls <- ecls %>% mutate(w3income_1k = w3income/1000)
m_ps <- glm(catholic ~ race_white + w3income_1k + p5hmage + p5numpla + w3momed_hsb,
            family = binomial(), data = ecls)
summary(m_ps)

# Resultado: todos son relevantes (significativos) para determinar si van a escuela privada o no,
# excepto "p5numpla".

# III.2 Ahora estimamos el propensity score (cuánta probabiliada le predice el modelo para estar en Tratamiento)
prs_df <- data.frame(pr_score=predict(m_ps, type="response"), # probabilidad calculada con estimadores
                  catholic = m_ps$model$catholic) #comparado con el dato real

head(prs_df)
# nota. Hay disparidades, ya que de acuerdo con características que yo dije (controles) qué prob. hay de que estén en grupo T
# Ej. Obs 4 y 5 tienen probabilidades similares y están en grupos distintos
# Ej. Obs 6 probabilidad alta pero no está en T

# IV. Analizar región del common support (verificar que se cumpla)
# graficamos histograma para el grupo de tratamiento y control
labs <- paste("Actual school type attended:",
              c("Catholic", "Public"))
labs
prs_df %>% 
  mutate(catholic=ifelse(catholic==1, labs[1], labs[2])) %>% 
  ggplot(aes(x = pr_score)) +
  geom_histogram(color='white') +
  facet_wrap(~catholic) +
  xlab("Probability of going to Catholic school") +
  theme_bw()

# Vemos que distribuciones son muy distintas. Los que van a escuela pública tenían muy pocas prob de ir a escuela pública (<.20)
# Sin embargo, esperaríamos que hist de escuela privada estuviera más sesgado hacia la izq. (más observaciones en la derecha)
# modelo logit no lo hace de manera perfecta, pero muchos sí están clasificados correctamente (lado derecho)

# Se cumple condificón de COMMON SUPPORT, i.e. ambas tienen mismo rango de probabilidades (0.0 a 0.5).
# Si esto no pasara, nunca podríamos empatar a los individuos.

# V. Utilice el procedimiento de matching mediante el criterio de la vecindad más cercana.

# V.0 omitimos n.a.
ecls_nomiss <- ecls %>% 
  select(c5r2mtsc_std, catholic, one_of(ecls_cov)) %>% 
  na.omit()

# V.1 Utilizamos funciones propias del paquete MatchIt:
mod_match <- matchit(catholic ~ race_white + w3income + p5hmage + p5numpla +w3momed_hsb,
                     method = "nearest", data = ecls_nomiss)

# generamos nuevo df con el resultado
dta_m <- match.data(mod_match)
dim(dta_m) # agrega variable distance, weights y subclass
head(dta_m)

# V.2 summary
summary(mod_match)


# V.3 gráficamos
plot(mod_match)

# VI. Evalúe el equilibrio de covariables después del matching
#VI.1 tabla comparando medias
dta_m %>% 
  group_by(catholic) %>% 
  select(one_of(ecls_cov)) %>% 
  summarise_all(funs(mean)) %>% 
  kable()

#VI.2 Prueba estadística apropiada, esperamos ahora NO RECHAZAR HO
lapply(ecls_cov, 
       function(v) {t.test(dta_m[, v] ~ dta_m$catholic)})

# VII. Estime los efectos del tratamiento sobre la variable de resultado
# VII.1 Estimamos efecto de tratamiento para conjunto de matching con t.test
with(dta_m, t.test(c5r2mtsc_std ~ catholic))
# El efecto existe y tenemos una medición más precisa de las medias para cada grupo
# con eso podemos identificar ATE para grupos emparejados

# VII.2 ATE emparejado ahora con recta de regresión (ASÍ PODEMOS CALCULAR ATE SIEMPRE)
lm_treat1 <- lm(c5r2mtsc_std ~ catholic, data = dta_m)
summary(lm_treat1)

# coinciden coeficientes:
# B0 = .359
# B1 = (.359 + (-.149)) = 0.209

# VII.3 ATE emparejado con controles
lm_treat2 <- lm(c5r2mtsc_std ~ catholic + race_white + p5hmage + I(w3income/1000) +
                  p5numpla + w3momed_hsb, data = dta_m)
summary(lm_treat2)
