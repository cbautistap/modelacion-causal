# Sesión 12. 
# Práctica 2. Variables instrumentales

library(AER)
library(haven)
library(tidyverse)

# 1. Cargar datos
data("CollegeDistance")
attach(CollegeDistance)
View(CollegeDistance)

# 2. Obtener descripción general del conjunto de datos
summary(CollegeDistance)
# Histograma para visualizar distancia
hist(distance)


# 3. Estime dos regresiones no confiables
# 3.1 
lwage 