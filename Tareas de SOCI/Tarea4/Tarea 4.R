####################################
# SOCI 4186 -- sección N           #
# Nombre:                          #
# Tarea 4: Análisis de Datos con R #
####################################

# En esta tarea harán lo siguiente:
# 1. Guardar este archivo, cambiando el nombre de Tarea4-4186.R
#    a Apellido1-Apellido2-Tarea4-{1 o 2}.R
# 2. Completar las próximas secciones y guardar la secuencia de comandos
#    que usaron para cumplir con lo pedido. Sigan el ejemplo mostrado aquí.

# Parte 0: Cargar datos ---------------------------------------------------
# Usarán el archivo 'encuesta_limpia.csv' disponible en la carpeta designada.

# Activar librerías necesarias
library(tidyverse)
library(dslabs)

# Modificar la ruta yendo a Session (arriba, seleccionando 'Set Working Directory')
# y seleccionando o creando la carpeta para poner todo lo de esta tarea.

# Cargar los datos de 'encuesta_limpia.csv'
datos <- read.csv("encuesta_limpia.csv") #ESTO PRESUME QUE LOS DATOS ESTÁN EN LA CARPETA

# Parte 1: Revisión inicial de datos -----------------------
# 1. Visualicen las primeras seis filas del conjunto de datos para familiarizarse.
head(datos)

#Las variables deben ser
# - sexo: Sexo del encuestado
# - edad: Grupo de edad
# - region: Región de residencia en Puerto Rico
# - ocupacion: Ocupación del encuestado (respuesta múltiple)
# - ingreso_mensual: Ingreso mensual promedio del hogar
# - atractivo_compras: Percepción de Río Piedras como atractivo para compras
# - atractivo_jangueo: Percepción de Río Piedras como atractivo para socializar
# - limpieza: Calificación de limpieza en el casco de Río Piedras (A a F)
# - ornato: Calificación de ornato en el casco de Río Piedras (A a F)
# - seguridad: Calificación de seguridad en el casco de Río Piedras (A a F)
# - opciones_compra: Calificación de opciones de compra en el casco de Río Piedras (A a F)
# - oferta_gastronomica: Calificación de oferta gastronómica en el casco de Río Piedras (A a F)
# - vida_nocturna: Calificación de vida nocturna en el casco de Río Piedras (A a F)
# - vista_general: Percepción general del casco urbano de Río Piedras
# - comparacion_anual: Comparación de percepción de Río Piedras respecto al año pasado
# - establecimientos: Establecimientos frecuentados en Río Piedras (respuesta múltiple)
# - redujo_gastos: Reducción de gastos en los últimos 12 meses por inflación
# - responsabilidad: Responsable de mantener Río Piedras en buen estado (respuesta múltiple)
# - comentarios: Comentarios adicionales del encuestado


# Parte 2: Creación de tablas ----------------------------------------------
# 1. Generen una tabla de frecuencias para la relación entre 'edad' y 'atractivo_jangueo'.
# (verifiquen MiSesión4.R como referencia), en este caso llenen los blancos
tabla_edad_jangueo <- table(argumento1,argumento2)
print(tabla_edad_jangueo)

# 2. Calcular porcentajes por fila en la tabla de 'edad' y 'atractivo_jangueo'.
porcentaje_edad_jangueo <- prop.table(tabla_edad_jangueo, margin = 1) * 100
print(round(porcentaje_edad_jangueo, 2))

# 3. Generen otra tabla de contingencia de su elección, incluyendo una variable de interés y otra
#    variable del dataset (e.g., 'sexo' vs 'atractivo_compras').

# Calcular porcentajes por fila para esta tabla extra.

# Parte 3: Creación de imágenes ---------------------------------------------
# 1. Generen un gráfico de barras apiladas para visualizar la relación 'edad' vs 'atractivo_jangueo'.
ggplot(datos, aes(x = X, fill = Y)) + #TIENEN QUE CAMBIAR LAS VARIABLES X E Y POR NOMBRE DE VARIABLE
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre edad y percepción de atractividad de Río Piedras para janguear",
       x = "Grupo etario",
       y = "Porcentaje",
       fill = "Atractividad de RP para Jangueo") +
  theme_minimal()

# 2. Generen otro gráfico de barras apiladas para la relación extra elegida.
# LES TOCA GENERAR PARA EL QUE CREARON USTEDES

# 3. Guarden las imágenes generadas como archivos PNG.
ggsave("barras_edad_jangueo.png", width = 8, height = 6)
ggsave("barras_sexo_compras.png", width = 8, height = 6)

# Parte 4: Tablas sumarias ---------------------------------------------

# Instala el paquete si no está instalado
library(stargazer)

# Cargar el dataset de asesinatos
data("murders", package = "dslabs")

# Crear la variable de tasa de asesinatos por cada 100,000 habitantes (LLENA EL BLANCO)
murders <- murders |>
  mutate(tasa_asesinato = ...... )|>
  rename(población = population)

# Generar una tabla de estadísticas descriptivas en LaTeX
stargazer(murders, type = "latex", 
          title = "Estadísticas descriptivas de asesinatos en EEUU", 
          summary = TRUE, 
          out = "tabla_descriptiva_asesinatos.tex")


# Parte 5: Guardar y entregar -----------------------------------------------
# 1. Asegúrense de que el código esté correctamente comentado y organizado.
# 2. Guardar el script con el nombre adecuado (Apellido1-Apellido2-Tarea4-{1 o 2}.R).
# 3. Compilen el script y asegúrense de que no haya errores.
# 4. Enviar el script y las imágenes generadas, y la tabla_descriptiva_asesinatos.tex
# a través del correo electrónico antes del 11 de noviembre de 2024.