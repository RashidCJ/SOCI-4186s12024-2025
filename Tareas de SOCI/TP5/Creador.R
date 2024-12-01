# Cargar librerías necesarias
library(ggplot2)

# Cargar los datos (ajusta el nombre del archivo y su ruta según corresponda)
data <- read.csv("encuesta_limpia.csv")

# Seleccionar las variables de interés
variables_of_interest <- c("ornato", "atractivo_jangueo", "edad", "region")
subset_data <- data[as.factor(variables_of_interest)]

# Crear una tabla resumen para las variables categóricas
summary_table <- summary(subset_data)

# Mostrar la tabla resumen
print(summary_table)

# Cargar librerías necesarias
library(tidyverse)

# Gráfico de barras para ornato
a<-datos |>
  ggplot(aes(x = ornato)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribución de calificaciones de ornato a Río Piedras",
       x = "Nota dada",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras para región
b<-datos |>
  ggplot(aes(x = region)) +
  geom_bar(fill = "darkorange", color = "black") +
  labs(title = "Distribución de vivienda reportada por encuestados",
       x = "Región",
       y = "Frecuencia") +
  theme_minimal()

# Gráfico de barras para edad
c<-datos |>
  ggplot(aes(x = edad)) +
  geom_bar(fill = "forestgreen", color = "black") +
  labs(title = "Distribución por Grupo Etario",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras para atractivo_jangueo
d<-datos |>
  ggplot(aes(x = atractivo_jangueo)) +
  geom_bar(fill = "red3", color = "black") +
  labs(title = "Distribución de atractivo para jangueo en Río Piedras",
       x = "Percepción de atractividad para jangueo",
       y = "Frecuencia") +
  theme_minimal()

# Instalar gridExtra si no lo tienes
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}

library(gridExtra)

# Disposición 2x2 con grid.arrange
grid.arrange(a, b, c, d, ncol = 2, nrow = 2)

#alternativamente, pueden poner dentro del título qué parte de la subimagen o subfigura trabajamos, e.g:
d<-datos |>
  ggplot(aes(x = atractivo_jangueo)) +
  geom_bar(fill = "red3", color = "black") +
  labs(title = "(d) Distribución de atractivo para jangueo en Río Piedras",
       x = "Percepción de atractividad para jangueo",
       y = "Frecuencia") +
  theme_minimal()
