# Sesión 06. Práctica ATE con datos LaLonde

library(Matching)
library(stargazer)

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
t.test(
  x = age[treat==1],
  y = age[treat==0],
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95)
)


 