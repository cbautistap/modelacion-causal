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

# I. 2. Significancia de diferencia en medias
with(ecls, t.test(c5r2mtsc_std ~ catholic))
# vemos que la diferencia sí es estadísticamente significativa.
# estadístico t es muy alto (valor absoluto) y por lo tanto, pvalue muy pequeño.
# SE RECHAZA H0: diferencia en medias igual a cero.