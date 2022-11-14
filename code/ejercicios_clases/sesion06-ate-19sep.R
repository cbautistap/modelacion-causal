# Sesión 06. Práctica ATE con datos LaLonde

library(Matching)
library(stargazer)
library(devtools) # sirve para poder leer problema desde RCT
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


# Paquete RCT

variables <- data.frame(age, educ, black, hisp, married, nodegr, treat)
tabla1 <- summary_statistics(variables)
stargazer(as.data.frame(tabla1), type="text", summary = FALSE)

# paquete trae función de tabla de balance:
tabla <- balance_table(variables, treatment = "treat")
stargazer(as.data.frame(tabla), type="text", summary = FALSE)
# vemos que se obtienen los mismos resultados que antes con t.test()

# Ojo. Variable "nodegr" sí tiene significancia (pvalue pequeño), sí influye en el tratamiento. 

#Mismo ejercicio con balance regression
tabla3 <- balance_regression(variables, treatment = "treat")
stargazer(as.data.frame(tabla3$regression_tables), type = "text", summary = FALSE)
# solo nodegr es significativo para el tratamiento (pvalue=0.006).
# como "nodegr" es significativa, podemos hacer 1) prueba de robustez para ver qué pasó
# 2) si con prueba de robustez confirmamos que sí es significativo, 
# estimar regresión de efecto causal con control (es control que dejas en el modelo)
# Nota. El intercepto es LA PROPORCIÓN DE NO TRATADOS. (promedio de variable dependiente)

# También podemos acceder a prueba F
stargazer(as.data.frame(tabla3$F_test), type = "text", summary = FALSE)
# F es el de la significancia conjunta. SIRVE PARA VERIFICAR SI ALEATORIZACIÓN DEPENDE O NO DE LOS CONTROLES
# k = grados de libertad
# F_critical = valor crítico
# pvalue
# Nota debe de coincidir con lo estimado con el método anterior (es el mismo modelo)

# Efecto tratamiento con controles
reg3 <- lm(re78 ~ treat+black) #
stargazer(reg1, reg3, type="text", summary = FALSE) # Comparamos reg3 con reg1
# Resultado. COEFICIENTE de black sí es significativo. Anteriormente no lo habíamos identificado
# además, cambia el coeficiente de treat

#Efecto de tratamiento con INTERACCIÓN
# se puede diferenciar el efecto de uno de los controles con la inclusión de una variable con interacción
reg4 <- lm(re78 ~ treat+black+I(treat*black)) #nota interacción genera cambio de pendiente cuando ocurren las dos
stargazer(reg4, type="text", summary = FALSE)
#Resultado. ESPERARÍA NO VER INTERACCIONES SIGNIFICATIVAS.
# Al no ser la interacción significativa, es otra forma de verificar que 
#tratamiento no está modificándose cuando ocurre black


# o bien, transformando respecto a su media a la dicotómica de control
reg5 <- lm(re78 ~ treat+I(black-mean(black)) # I() es la indicadora que permite hacer operación que marcas
           +I(treat*(black-mean(black))))
stargazer(reg5, type = "text", summary = FALSE)
# Nota. Esta transformación es útil porque permite interpretar resultado respecto a unidades de
# desviación de la media. Nos permiten medir el efecto, qué tantas veces respecto de su media se va a
# modificar la variable de respuesta ante cambios en la independiente específica. Black tiene media cero
# estandarizar en media cero permite identificar más facil.
# En resultado. Coeficientes de interacción son los mismos, cambia el de la constante
# Resultado. Variables
# Resultado, 