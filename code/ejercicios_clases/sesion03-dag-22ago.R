# Sesión 03. DAGs (22-08-2022)
# Taller 01.

library("dagitty")

g1 <- dagitty('dag{
              "Cámara" -> "Atención" -> "Aprendizaje"
              "Cámara" -> "Dinamismo" -> "Aprendizaje"
              "Cámara" -> "Conexión" -> "Aprendizaje"
              "Dinamismo" -> "Atención"
              }')

plot(g1)
