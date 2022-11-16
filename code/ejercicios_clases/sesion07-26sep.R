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
