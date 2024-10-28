###########################
# SOCI 4186 -- sección N  #
# Nombre:                 #
# Tarea 4: Análisis de Datos con R #
###########################

# En esta tarea harán lo siguiente:
# 1. Guardar esta tarea, cambiando el nombre de Tarea4-4186.R
#    a Apellido1-Apellido2-Tarea4-{1 o 2}.R
# 2. Completar las próximas secciones, guardando la secuencia
#    de comandos que usaron para cumplir con lo pedido. Continúen desde el ejemplo.

# Parte 0: Cargar datos ---------------------------------------------------
# Activarán el paquete 'dslabs', y cargarán el conjunto de datos 'heights' 
# que contiene información sobre las alturas y sexos de un grupo de individuos.

library(dslabs)
data("heights")

# Parte 1: Revisión y manipulación inicial de datos -----------------------
# 1. Visualicen las primeras seis filas del conjunto de datos.
head(heights)

# 2. Creen una variable adicional que clasifique las alturas en categorías:
#    'Baja' (< 160 cm), 'Media' (160-180 cm), 'Alta' (> 180 cm).
heights$altura_categoria <- cut(
  heights$height,
  breaks = c(-Inf, 60, 70, Inf),
  labels = c("Baja", "Media", "Alta")
)

# Parte 2: Creación de objetos ----------------------------------------------
# 1. Crear un objeto que almacene el número de individuos en cada categoría de altura.
conteo_categorias <- table(heights$altura_categoria)
conteo_categorias

# 2. Crear un objeto que almacene la altura promedio por sexo.
altura_promedio_sexo <- heights %>%
  group_by(sex) %>%
  summarize(promedio = mean(height, na.rm = TRUE))
altura_promedio_sexo

# Parte 3: Creación de tablas -----------------------------------------------
# 1. Generen una tabla de contingencia que muestre la distribución de sexo por categoría de altura.
tabla_contingencia <- table(heights$sex, heights$altura_categoria)
tabla_contingencia

# 2. Conviertan la tabla de contingencia en un data frame para facilitar su manipulación.
df_contingencia <- as.data.frame(tabla_contingencia)
colnames(df_contingencia) <- c("Sexo", "Categoría_Altura", "Frecuencia")
df_contingencia

# Parte 4: Creación de imágenes ---------------------------------------------
# 1. Generen un histograma de las alturas, diferenciando por sexo usando colores distintos.
library(ggplot2)

ggplot(heights, aes(x = height, fill = sex)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de Alturas por Sexo",
       x = "Altura (cm)",
       y = "Frecuencia") +
  theme_minimal()

# 2. Generen un diagrama de barras para la tabla de contingencia creada anteriormente.
ggplot(df_contingencia, aes(x = Categoría_Altura, y = Frecuencia, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribución de Sexo por Categoría de Altura",
       x = "Categoría de Altura",
       y = "Frecuencia") +
  theme_minimal()

# 3. Guardar las imágenes generadas como archivos PNG.
ggsave("histograma_alturas_sexo.png", width = 8, height = 6)
ggsave("barras_contingencia_sexo_altura.png", width = 8, height = 6)

# Parte 5: Análisis adicional -----------------------------------------------
# 1. Calcular la mediana de las alturas para cada categoría de altura.
mediana_altura_categoria <- heights %>%
  group_by(altura_categoria) %>%
  summarize(mediana = median(height, na.rm = TRUE))
mediana_altura_categoria

# 2. Identificar si hay una diferencia significativa en las alturas promedio entre sexos.
#    Realizar una prueba t y reportar el valor p.
t_test_result <- t.test(height ~ sex, data = heights)
t_test_result

# Parte 6: Guardar y entregar -----------------------------------------------
# 1. Asegúrense de que todo el código esté correctamente comentado y organizado.
# 2. Guardar el script con el nombre adecuado (Apellido1-Apellido2-Tarea4-{1 o 2}.R).
# 3. Compilen el script y asegúrense de que no haya errores.
# 4. Enviar el script y las imágenes generadas a través del método designado 
#    antes de la fecha de entrega.

# Nota: Las imágenes deben estar guardadas en el mismo directorio que el script
#       o especificar la ruta correcta al guardarlas.