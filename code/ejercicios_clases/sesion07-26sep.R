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

# Obtenemos el estimador con la diferencia de medias.
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
