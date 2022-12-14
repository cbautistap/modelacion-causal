# Sesión 03. DAGs (22-08-2022)
# Taller 01.

library("dagitty")
library("rethinking")

# La idea del paquete es construir DAGs
# cómo se relacionan los nodos a partir de las flechas

# Ejemplo 1.
g1 <- dagitty('dag{
              "Cámara" -> "Atención" -> "Aprendizaje"
              "Cámara" -> "Dinamismo" -> "Aprendizaje"
              "Cámara" -> "Conexión" -> "Aprendizaje"
              "Dinamismo" -> "Atención"
              }')

plot(g1)

# Ejemplo 2.
# para lograr mayor claridad, se pueden definir las posiciones de cada vértice como atributos.
# (ojo, también se pueden ver con coordinates() - ver ?coordinates -)

g2 <- dagitty('dag{
              "Cámara" [pos="0,1"]
              "Atención" [pos="1,0"]
              "Dinamismo" [pos="1,1"]
              "Conexión" [pos="1,2"]
              "Aprendizaje" [pos="2,1"]
              "Cámara" -> "Atención" -> "Aprendizaje"
              "Cámara" -> "Dinamismo" -> "Aprendizaje"
              "Cámara" -> "Conexión" -> "Aprendizaje"
              "Dinamismo" -> "Atención"
}')

plot(g2)

# Ejemplo 3. Incorpora nueva variable (privacidad y participación)
g3 <- dagitty('dag{
              "Cámara" [pos="0,1"]
              "Atención" [pos="1,0.5"]
              "Dinamismo" [pos="1,0"]
              "Conexión" [pos="1,1"]
              "Aprendizaje" [pos="2,1"]
              "Privacidad" [pos="0.5, 1.5"]
              "Participación" [pos="1,2"]
              "Cámara" -> "Atención" -> "Aprendizaje"
              "Cámara" -> "Dinamismo" -> "Aprendizaje"
              "Cámara" -> "Conexión" -> "Aprendizaje"
              "Cámara" -> "Privacidad" -> "Participación" -> "Aprendizaje"
              "Dinamismo" -> "Atención"
              "Conexión" -> "Participación"
}')
 
plot(g3)

# Mejorar formato con librería "rethinking"

drawdag(g3, col_labels= "blue", col_arrow = "red")

# Funciones adicionales de dagitty que me muestran las relaciones.
# Quienes lo causan.
parents(g3, "Aprendizaje") #variables que tienen efecto DIRECTO
ancestors(g3, "Aprendizaje") #variables que tienen algún tipo de efecto
# a quién causa
children(g3, "Cámara") #variables causadas de manera inmediata
descendants(g3, "Cámara") #variables causadas de cualquier tipo

