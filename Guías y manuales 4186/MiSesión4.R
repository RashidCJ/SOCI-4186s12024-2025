# Instalar y cargar paquetes necesarios
# install.packages("tidyverse")  # Ejecuta esto si no tienes instalado tidyverse
library(tidyverse)
library(readxl)    # Para leer archivos Excel
library(lubridate) # Para manejar fechas y horas
library(stargazer) # Para generar tablas en LaTeX

# 1. Importar los datos
# Reemplaza 'Encuesta economía Río Piedras (respuestas).xlsx' con la ruta correcta a tu archivo de datos
#datos <- read_excel("Encuesta economía Río Piedras (respuestas).xlsx")
datos <- read_excel("Nueva encuestita para clase Economía Río Piedras.xlsx")
summary(datos)
#Eliminar fila si en Hoja de Consentimiento dijeron 'Declino'
datos <- subset(datos, `Hoja de Consentimiento` != 'Declino')

# 2. Eliminar columnas innecesarias
# Eliminamos 'Hoja de Consentimiento', 'Puntuación' y 'Marca temporal'
datos <- datos %>%
  select(-c(`Hoja de Consentimiento`, `Puntuación`, `Marca temporal`))

# 3. Renombrar columnas para facilitar su manejo
datos <- datos %>%
  rename(
    sexo = `¿Cuál es su sexo?`,
    edad = `¿Cuál es tu grupo de edad?`,
    region = `¿En qué región de Puerto Rico vive?`,
    ocupacion = `¿A qué se dedica? (puede escoger más de una opción)`,
    ingreso_mensual = `¿Cuál es el ingreso mensual promedio aproximado de su hogar?`,
    atractivo_compras = `¿Cree que Río Piedras se ve atractivo para ir de compras?`,
    atractivo_jangueo = `¿Cree que Río Piedras se ve atractivo para janguear con amistades?`,
    limpieza = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Limpieza]`,
    ornato = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Ornato]`,
    seguridad = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Seguridad]`,
    opciones_compra = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Opciones de compra]`,
    oferta_gastronomica = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Oferta gastronómica]`,
    vida_nocturna = `Calificando de la A a la F, ¿cómo entiende que es el casco de Río Piedras en los siguientes renglones? [Vida nocturna]`,
    vista_general = `¿Cómo se ve el casco urbano de Río Piedras en conjunto?`,
    comparacion_anual = `¿Cree que Río Piedras se ve mejor, igual, o peor que hace un año?`,
    establecimientos = `¿En Río Piedras, cuál de los siguientes establecimientos gusta frecuentar?`,
    redujo_gastos = `¿Ha tenido que reducir sus gastos en los últimos 12 meses debido a la inflación?`,
    responsabilidad = `¿A quién le corresponde tener a Río Piedras en buen estado?`,
    comentarios = `Gracias por participar. Si tiene comentarios no dude en dejarme saber rellenando la casilla de abajo.`
  )

# 4. Convertir variables categóricas a factores
datos <- datos %>%
  mutate_at(vars(sexo, edad, region, ingreso_mensual, atractivo_compras, atractivo_jangueo,
                 limpieza, ornato, seguridad, opciones_compra, oferta_gastronomica,
                 vida_nocturna, vista_general, comparacion_anual, redujo_gastos), as.factor)

summary(datos)

datos$establecimientos <- tolower(trimws(datos$establecimientos))

# Ver datos con nombres normalizados
print(datos)
# 5. Manejar respuestas múltiples en 'ocupacion', 'responsabilidad' y 'establecimientos'
# Separar las respuestas múltiples en filas individuales
datos_ocupacion <- datos %>%
  separate_rows(ocupacion, sep = ",\\s*")

datos_responsabilidad <- datos %>%
  separate_rows(responsabilidad, sep = ",\\s*")

datos_establecimientos <- datos %>%
  separate_rows(establecimientos, sep = ",\\s*")

# Crear variables binarias para cada ocupación
datos_ocupacion_bin <- datos_ocupacion %>%
  mutate(valor = 1) %>%
  pivot_wider(names_from = ocupacion, values_from = valor, values_fill = list(valor = 0))

# Crear variables binarias para cada responsabilidad
datos_responsabilidad_bin <- datos_responsabilidad %>%
  mutate(valor = 1) %>%
  pivot_wider(names_from = responsabilidad, values_from = valor, values_fill = list(valor = 0))

# Crear variables binarias para cada establecimiento
datos_establecimientos_bin <- datos_establecimientos %>%
  mutate(valor = 1) %>%
  pivot_wider(names_from = establecimientos, values_from = valor, values_fill = list(valor = 0))

# Unir nuevamente todas las variables binarias al conjunto de datos principal
datos <- datos %>%
  left_join(datos_ocupacion_bin, by = c("sexo", "edad", "region", "ingreso_mensual", "atractivo_compras",
                                        "atractivo_jangueo", "limpieza", "ornato", "seguridad",
                                        "opciones_compra", "oferta_gastronomica", "vida_nocturna",
                                        "vista_general", "comparacion_anual", "redujo_gastos",
                                        "comentarios"))

datos <- datos %>%
  left_join(datos_responsabilidad_bin, by = c("sexo", "edad", "region", "ingreso_mensual", "atractivo_compras",
                                              "atractivo_jangueo", "limpieza", "ornato", "seguridad",
                                              "opciones_compra", "oferta_gastronomica", "vida_nocturna",
                                              "vista_general", "comparacion_anual", "redujo_gastos",
                                              "comentarios"))

datos <- datos %>%
  left_join(datos_establecimientos_bin, by = c("sexo", "edad", "region", "ingreso_mensual", "atractivo_compras",
                                               "atractivo_jangueo", "limpieza", "ornato", "seguridad",
                                               "opciones_compra", "oferta_gastronomica", "vida_nocturna",
                                               "vista_general", "comparacion_anual", "redujo_gastos",
                                               "comentarios"))

# 6. Recodificar y estandarizar categorías
# Unificar respuestas similares en 'region'
datos <- datos %>%
  mutate(region = recode(region,
                         "San Juan, Santurce o Río Piedras" = "San Juan",
                         "Área metropolitana, excepto municipio de San Juan" = "Área metropolitana",
                         "Fuera del área metropolitana de San Juan" = "Fuera del área metropolitana"))

# 7. Manejar valores faltantes y duplicados
# Eliminar filas duplicadas
datos <- datos %>%
  distinct()

# 8. Análisis exploratorio inicial
# Resumen estadístico
summary(datos)

# Tablas de frecuencia para variables categóricas
table(datos$sexo)
table(datos$region)
table(datos$edad)

# 9. Visualización de datos
# Gráfico de barras de sexo
ggplot(datos, aes(x = sexo)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Distribución por Sexo", x = "Sexo", y = "Cantidad")

# 10. Guardar el conjunto de datos limpio
write_csv(datos, "encuesta_limpia.csv")

# 11. Tablas de frecuencias y porcentajes de las relaciones entre nuestros datos

# Relación entre 'sexo' y 'redujo_gastos'
tabla_sexo_gastos <- table(datos$sexo, datos$redujo_gastos)
print(tabla_sexo_gastos)

# Calcular porcentajes por fila
porcentaje_sexo_gastos <- prop.table(tabla_sexo_gastos, margin = 1) * 100
print(round(porcentaje_sexo_gastos, 2))

# Gráfico de barras apiladas para visualizar la relación
ggplot(datos, aes(x = sexo, fill = redujo_gastos)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre Sexo y Reducción de Gastos",
       x = "Sexo",
       y = "Porcentaje",
       fill = "¿Redujo Gastos?") +
  theme_minimal()

# Relación entre 'edad' y 'atractivo_compras'
tabla_edad_atractivo <- table(datos$edad, datos$atractivo_compras)
print(tabla_edad_atractivo)

# Porcentajes por fila
porcentaje_edad_atractivo <- prop.table(tabla_edad_atractivo, margin = 1) * 100
print(round(porcentaje_edad_atractivo, 2))

# Gráfico de barras apiladas
ggplot(datos, aes(x = edad, fill = atractivo_compras)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Relación entre Edad y atractivo de Río Piedras para ir de compras",
       x = "Grupo de Edad",
       y = "Porcentaje",
       fill = "Atractividad para compras") +
  theme_minimal()

# 12. Pruebas de Ji cuadrado para analizar relaciones entre variables

# Prueba entre 'sexo' y 'redujo_gastos'
chi_sexo_gastos <- chisq.test(tabla_sexo_gastos)
print(chi_sexo_gastos)

# Verificar si se cumplen las condiciones para la prueba de Ji cuadrado
if(any(chi_sexo_gastos$expected < 5)) {
  # Si alguna frecuencia esperada es menor que 5, usar la prueba exacta de Fisher
  fisher_sexo_gastos <- fisher.test(tabla_sexo_gastos)
  print(fisher_sexo_gastos)
}

# Prueba entre 'edad' y 'atractivo_compras'
chi_edad_atractivo <- chisq.test(tabla_edad_atractivo)
print(chi_edad_atractivo)

if(any(chi_edad_atractivo$expected < 5)) {
  fisher_edad_atractivo <- fisher.test(tabla_edad_atractivo)
  print(fisher_edad_atractivo)
}

# Prueba entre dummies creadas y respuestas
# Por ejemplo, 'Estudiante universitario' y 'redujo_gastos'

# Asegurarse de que la variable dummy exista y esté en formato factor
if("Estudiante universitario" %in% names(datos)) {
  datos$`Estudiante universitario` <- as.factor(datos$`Estudiante universitario`)
  
  tabla_estudiante_gastos <- table(datos$`Estudiante universitario`, datos$redujo_gastos)
  print(tabla_estudiante_gastos)
  
  chi_estudiante_gastos <- chisq.test(tabla_estudiante_gastos)
  print(chi_estudiante_gastos)
  
  if(any(chi_estudiante_gastos$expected < 5)) {
    fisher_estudiante_gastos <- fisher.test(tabla_estudiante_gastos)
    print(fisher_estudiante_gastos)
  }
}

# 13. Usar stargazer para generar tablas para LaTeX

# Generar una tabla resumen de estadísticas descriptivas
stargazer(datos, type = "text", summary = TRUE)

# Generar tablas de contingencia en formato LaTeX
# Convertir tablas a data frames para stargazer
df_tabla_sexo_gastos <- as.data.frame.matrix(tabla_sexo_gastos)
df_tabla_edad_atractivo <- as.data.frame.matrix(tabla_edad_atractivo)

# Tabla de 'sexo' y 'redujo_gastos'
stargazer(df_tabla_sexo_gastos, type = "latex", summary = FALSE,
          title = "Tabla de Contingencia: Sexo vs Reducción de Gastos",
          label = "tab:sexo_gastos")

# Tabla de 'edad' y 'atractivo_compras'
stargazer(df_tabla_edad_atractivo, type = "latex", summary = FALSE,
          title = "Tabla de Contingencia: Edad vs Atractivo para Compras",
          label = "tab:edad_atractivo")

# Generar resultados de pruebas de Ji cuadrado en formato LaTeX
# Crear un data frame con los resultados
resultados_chi <- data.frame(
  Prueba = c("Sexo vs Reducción de Gastos", "Edad vs Atractivo para Compras"),
  Estadístico = c(chi_sexo_gastos$statistic, chi_edad_atractivo$statistic),
  Grados_de_Libertad = c(chi_sexo_gastos$parameter, chi_edad_atractivo$parameter),
  Valor_p = c(chi_sexo_gastos$p.value, chi_edad_atractivo$p.value)
)

# Generar tabla con stargazer
stargazer(resultados_chi, type = "latex", summary = FALSE,
          title = "Resultados de Pruebas de Ji Cuadrado",
          label = "tab:resultados_chi",
          digits = 4)

# Guardar tablas en archivos .tex si es necesario
# Por ejemplo:
stargazer(df_tabla_sexo_gastos, type = "latex", summary = FALSE,
          title = "Tabla de Contingencia: Sexo vs Reducción de Gastos",
          label = "tab:sexo_gastos",
          out = "tabla_sexo_gastos.tex")

# Puedes hacer lo mismo para otras tablas y resultados.