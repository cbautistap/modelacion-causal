# Sesión 06. Práctica ATE con datos LaLonde

library(Matching)
library(stargazer)
library(devtools)
library(RCT)

data(lalonde)
attach(lalonde) #Añade datos a R reading path, permite llamar variables con su nombre
View(lalonde)

#Calculamos estimador para el efecto del tratamiento t_gorro = yt_gorro - yc_gorro
estimador <- mean(re78[treat==1]) - mean(re78[treat==0]) # estimador de Neyman

# Pero queremos probar si el efecto es significativo o no (H0 = t_gorro = 0 vs H1 = t_gorro =! 0)
# Con el cálculo de la varianza de Neyman. Fórmula de Varianza
var_estimador <- var(re78[treat==1])/length(re78[treat==1]) + var(re78[treat==0])/length(re78[treat==0])

# Este mismo estimador puede obtenerse mediante la regresión por MCO de la forma
# yi = B0 + B1Ti + ui
reg1 <- lm(re78 ~ treat)
stargazer(reg1, type="text")


# Verificamos independencia de los resultados.
# Constuir tabla de balance. Permite identificar si las diferencias entre las medias de las variables
# de control para cada grupo son o no iguales. Siempre se busca tener una tabla balanceada. Por ejemplo,
# para el control age:
c(mean(age[treat==1]), mean(age[treat==0])) # esperamos que sean iguales..
# si no son significativamente diferentes, aleatorización bien hecha.

# Prueba de hipótesis que permite comparar ambas medias:
# para variable age:
t.test(
  x = age[treat==1],
  y = age[treat==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95) # p-value no significativo. ESO ESPERAMOS!

# hacemos lo mismo pero para otros controles. Ej variable educ
t.test(
  x = educ[treat==1],
  y = educ[treat==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95)  # p-value no significativo. ESO ESPERAMOS!
# ADEMÁS, valor de H0 = 0 cae dentro del intervalo

# Control black
t.test(
  x = black[treat==1],
  y = black[treat==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95)  # p-value no significativo. ESO ESPERAMOS!
# ADEMÁS, valor de H0 = 0 cae dentro del intervalo

# Control black
t.test(
  x = hisp[treat==1],
  y = hisp[treat==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95)  # p-value no significativo, pero apenitas pvalue = 0.064.
# En hispanos, lo que pasa es una prueba muy pequeña
# ADEMÁS, valor de H0 = 0 cae dentro del intervalo


# POR LO TANTO, podemos decir que aleatorización fue adecuada, como lo acabamos de probar.

# Prueba F en MLP
# prueba de significancia conjunta (prueba F)
mlp <- lm(treat ~ age + educ + black)
stargazer(mlp, type="text")
