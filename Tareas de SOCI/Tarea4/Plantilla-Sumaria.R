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

#Asumiendo que los datos son complicados o nada numéricos esto podría ser un acercamiento:
#asumiendo que han cargado los datos de encuestas como datos 
datos_seleccion <- datos |>
  select(edad, ingreso_mensual, atractivo_compras, atractivo_jangueo) |>
  mutate(
    atractivo_compras = as.factor(atractivo_compras),
    atractivo_jangueo = as.factor(atractivo_jangueo)
  )

# Verificar si hay variables numéricas
datos_numericos <- datos_seleccion %>% select(where(is.numeric))

# Generar tabla de estadísticas descriptivas solo si hay variables numéricas
if (ncol(datos_numericos) > 0) {
  stargazer(datos_numericos, 
            type = "latex", 
            title = "Estadísticas Descriptivas de Variables Numéricas Seleccionadas", 
            summary = TRUE, 
            out = "tabla_descriptiva_numericas.tex")
} else {
  cat("No hay variables numéricas seleccionadas para generar una tabla de estadísticas descriptivas.\n")
}

# Generar tablas de frecuencia para variables categóricas
if ("atractivo_compras" %in% names(datos_seleccion)) {
  frecuencia_compras <- table(datos_seleccion$atractivo_compras)
  print("Frecuencia de Atractivo para Compras:")
  print(frecuencia_compras)
}

if ("atractivo_jangueo" %in% names(datos_seleccion)) {
  frecuencia_jangueo <- table(datos_seleccion$atractivo_jangueo)
  print("Frecuencia de Atractivo para Jangueo:")
  print(frecuencia_jangueo)
}

# Crear una tabla de contingencia entre 'atractivo_compras' y 'atractivo_jangueo'
tabla_contingencia <- table(datos$atractivo_compras, datos$atractivo_jangueo)

# Imprimir la tabla de contingencia
print("Tabla de contingencia entre Atractivo para Compras y Jangueo:")
print(tabla_contingencia)

# Convertir la tabla de contingencia en un data frame para manipulación adicional (opcional)
df_contingencia <- as.data.frame(tabla_contingencia)
colnames(df_contingencia) <- c("Atractivo_Compras", "Atractivo_Jangueo", "Frecuencia")

# Mostrar el data frame
print(df_contingencia)
