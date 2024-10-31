#Plantilla de ejemplo: Creación de tablas

# Instala el paquete si no está instalado
if (!require(stargazer)) install.packages("stargazer")
library(stargazer)

# Carga un dataset de ejemplo (puedes cambiarlo por tu otro conjunto de datos, como murders o heights, o el suyo)
data("mtcars")

# Genera una tabla de estadísticas descriptivas simple
stargazer(mtcars, type = "text", title = "Estadísticas Descriptivas del Dataset", 
          summary = TRUE, out = "tabla_descriptiva.tex")

# La misma pero para salida con LaTeX
stargazer(mtcars, type = "latex", title = "Estadísticas Descriptivas del Dataset", 
          summary = TRUE, out = "tabla_descriptiva.tex")

# Esto generará una tabla con la media, desviación estándar, mínimo y máximo de cada variable
