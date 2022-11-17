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


# III. Estime propensity score 
# como probabilidad de recibir tratamiento dado un conjunto de covariables previas al tratamiento

# IV. Analizar región del common support (verificar que se cumpla)

# V. Utilice el procedimiento de matching mediante el criterio de la vecindad más cercana

# VI. Evalúe el equilibrio de covariables después del matching

# VII. Estime los efectos del tratamiento sobre la variable de resultado