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
# Transformaciones
CollegeDistance <- CollegeDistance %>% 
  mutate(hispanic = case_when(ethnicity=="hispanic" ~ 1, TRUE ~ 0),
         afam = case_when(ethnicity=="afam" ~ 1, TRUE ~ 0),
         female = case_when(gender=="female" ~ 1, TRUE ~ 0),
         urban = case_when(urban=="yes" ~1, TRUE~ 0),
         lwage = log(wage),
         leduc = log(education))

attach(CollegeDistance)

summary(CollegeDistance)

# Histograma para visualizar distancia
hist(distance)


# 3. Estime dos regresiones no confiables
# 3.1 
reg1_nc <- lm(lwage ~ leduc) 
summary(reg1_nc)

reg2_nc <- lm(lwage ~ leduc + hispanic + afam + female + urban)
summary(reg2_nc)

# 4. Porqué se puede utilizar la distancia a la escuela como instrumento?

# 5. Calcule correlaciones de la distancia del instrumento con la regresora endógena
# y la variable dependiente del salario.
cor.test(distance, leduc)
cor.test(distance, lwage)

# 6. 