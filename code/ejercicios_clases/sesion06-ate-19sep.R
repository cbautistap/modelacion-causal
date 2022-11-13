# Sesión 06. Práctica ATE con datos LaLonde

library(Matching)

data(lalonde)
attach(lalonde) #Añade datos a R reading path, permite llamar variables con su nombre
View(lalonde)

#Calculamos estimador para el efecto del tratamiento t_gorro = yt_gorro - yc_gorro
mean(re78[treat==1]) - mean(re78[treat==0]) # estimador de Neymann

# Pero queremos probar si el efecto es significativo o no (H0 = t_gorro = 0 vs H1 = t_gorro =! 0)