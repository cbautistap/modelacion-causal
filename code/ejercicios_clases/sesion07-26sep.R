# Sesión 07 (clase presencial). ATE Bajo estratificación
# Ejercicio con datos titanic.


library(tidyverse)
library(dagitty)
#install.packages(c("coda", "mvtnorm", "devtools", "loo", "dagitty", "shape"))
#devtools::install_github("rmcelreath/rethinking")
library(rethinking)
#install.packages("haven")
library("haven")
library("magrittr")
library("stargazer")

g <- dagitty('dag{
             "Mujer" [pos="1,0.5"]
             "Primera Clase" [pos="1,1"]
             "Sobrevivir" [pos="2,1"]
             "Menor" [pos="1,1.5"]
             "Primera Clase" -> "Sobrevivir"
             "Primera Clase" <- "Mujer" -> "Sobrevivir"
             "Primera Clase" <- "Menor" -> "Sobrevivir"}')

#plot(g)
drawdag(g, col_labels="blue", col_arrow="red", lwd=1.5, cex=1.2)

titanic <- read_dta("data/titanic.dta")
attach(titanic)
View(titanic)

# Iniciamos estimando controlando solo por la clase en qué viajaba el pasajero
titanic <- titanic %>% mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0)) # TRUE forces case_when to output the
                                                            # "else-output-value", if none of the
                                                            # previous conditions were TRUE.
                                                            # formula for everything else.
distinct(titanic, d)

# I. Obtenemos el estimador con la diferencia de medias.
# DOS FORMAS DE HACERLO
# 1. CBP
mean(titanic$survived[titanic$d == 1])
mean(titanic$survived[titanic$d == 0])
# 2. TIDYVERSE
ey1 <- titanic %>% filter(d == 1) %>% 
        pull(survived) %>% mean()
ey0 <- titanic %>% filter(d==0) %>% 
        pull(survived) %>% mean()
gate <- ey1 - ey0
print(gate)
 # gate nos dice que haber estado como pasajero en primera clase, la probabilidad 
 # de sobrevivir aumentaba en un 35.38%. Sin embargo, esta estimación del ATE está
 # sesgada, pues no hemos controlado por otros determinantes como edad y género

# II. Estratificación.
# 1. Estratificamos los datos en cuatro grupos
# g1: niños hombres, g2: niñas mujeres, g3: adultos hombre, g4: adultas mujeres
titanic <- titanic %>% 
  mutate(s = case_when(sex == 0 & age == 1 ~ 1,
                       sex == 0 & age == 0 ~ 2,
                       sex == 1 & age == 1 ~ 3,
                       sex == 1 & age == 0 ~ 4,
                       TRUE ~ 0))
distinct(titanic, s)

# 2. Calculamos diferencia de probabilidades de supervivencia para cada grupo
# 2.a Probabilidades de sobrevivencia para cada grupo
ey11 <- titanic %>% filter(s == 1 & d == 1) %>% #grupo 1 y clase alta
  pull(survived) %>% mean()
ey10 <- titanic %>% filter(s == 1 & d == 0) %>% #grupo 1 y clase no alta
  pull(survived) %>% mean()
ey21 <- titanic %>% filter(s == 2 & d == 1) %>% 
  pull(survived) %>% mean()
ey20 <- titanic %>% filter(s == 2 & d == 0) %>% 
  pull(survived) %>% mean()
ey31 <- titanic %>% filter(s == 3 & d == 1) %>%
  pull(survived) %>% mean()
ey30 <- titanic %>% filter(s==3 & d==0) %>% 
  pull(survived) %>% mean()
ey41 <- titanic %>% filter(s == 4 & d ==1) %>% 
  pull(survived) %>% mean()
ey40 <- titanic %>% filter(s == 4 & d == 0) %>% 
  pull(survived) %>% mean()

# 2.b Diferencia de probabilidad de sobrevivencia para cada grupo
diff1 <- ey11 - ey10
diff2 <- ey21 - ey20
diff3 <- ey31 - ey30
diff4 <- ey41 - ey40

print(c(diff1, diff2, diff3, diff4))

# 3. Cálculo de Ponderadores para afinar estimación
# Número de personas por cada grupo que NO SON DE CLASE ALTA y dividir por el total
obs_nca <- nrow(titanic %>% filter(d==0)) # total de pasajeros que NO SON CLASE ALTA
w1 <- titanic %>% filter(s==1 & d==0) %>%  
  nrow(.)/obs_nca #ponderador g1
w2 <- titanic %>% filter(s==2 & d==0) %>% 
  nrow(.)/obs_nca 
w3 <- titanic %>% filter(s==3 & d==0) %>% 
  nrow(.)/obs_nca
w4 <- titanic %>% filter(s==4 & d==0) %>% nrow(.)/obs_nca

# 4. Estimación de Weighted ATE
wate <- diff1*w1 + diff2*w2 + diff3*w3 + diff4*w4
print(wate)

# III. Comparamos ATE y Weitghted ATE (estratificado)
stargazer(gate, wate, type= "text")
