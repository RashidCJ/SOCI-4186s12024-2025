#Empezamos cargando paquetes que usaremos hoy

library(tidyverse)
library(dslabs)

# La idea hoy es cubrir dos temas: cargar y guardar datos.
#La importación de datos está cubierta en el capítulo 5 del libro de Irizarry
# disponible en https://leanpub.com/dslibro 

#hemos cubierto previamente cómo cargar datos pre-existentes en paquetes,
#los llamamos con la función data():
data(murders)
summary(murders)
#y podemos ver sus datos por lo general resumidos (en especial los numéricos)
#con la función summary() arriba.

#Vale recordar que R es un lenguaje enfocado en objetos o sus partes, y las 
#funciones necesitan indicarles el objeto, p.ej. summary(objeto)

#Ahora, hay varias formas que las que podríamos abrir documentos varios,
#como aquellos donde guardemos nuestros datos recopilados (como Excel, CSV).

#Primero toca entender las rutas del sistema y dónde están localizados los mismos

system.file(package = "dslabs") #entendiendo rutas de paquetes y sus archivos
dir<-system.file(package = "dslabs") #entendiendo rutas
list.files(dir)
#setwd("~/Documentos/UPRRP/ClasesIUPI/SOCI 4186/R4186") #escribiendo la ruta
getwd()
wd<-getwd()

#del internet sin paquete
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

dat <- read_csv(url)

filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs")
fullpath <- file.path(dir, filename)
dir <- system.file(package = "dslabs")
filename %in% list.files(file.path(dir, "extdata"))

# escribiendo archivos (#separado por...)
write_csv(murders, "murders.csv") #coma
#write_csv2(murders, "murders.csv") #punto y coma
write_tsv(murders, "murders.tsv") #tab
write.table(murders, "murders.txt") #espacios
#para escribir en formato Excel llamamos al paquete writexl y readxl,
#recuerden instalarlo si no está entre la lista de paquetes instalados
library(writexl)
library(readxl)
write_xlsx(murders,"murders.xlsx")

# cargando un csv
murders <- read_csv("murders.csv")
#cargando csv pero por ; 
read_csv2("murders.csv") #¿qué pasa si es el formato errado?

#verificar líneas
read_lines("murders.csv",n_max=4)
objeton<-file.path(wd, "murders.csv") #podriamos usar la dirección entera
murders<-read_csv2(objeton)

#visualización general
#primero prepararé la información tal cuál la teníamos (final de MiSesión2.R)

#Componentes de gráficos:
# 1. datos
# 2. geometrías
# 3. mapeo estético (representación estética en el mapa)
#en ggplot esto se pasa en varios argumentos usando la gramática de gráficos
murders<-murders %>% 
  mutate(tasa=sum(total) / sum(population) * 10^5)

murdersg<-murders %>%
  mutate(grupo = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Nueva Inglaterra",
    abb %in% c("WA", "OR", "CA") ~ "Costa del Pacífico",
    region == "South" ~ "el Sur",
    TRUE ~ "Otras regiones")) %>%
  group_by(grupo) %>%
  summarise(tasa_100k = sum(total)/ sum(population) * 10^5)
#gráficas circulares (y porqué son subóptimas en general)
pie(murdersg$tasa_100k)

murdersg
pie(murdersg$tasa_100k, labels = c("Costa del Pacífico (¿?)","Nueva Inglaterra","Otras regiones","el Sur (¿?)"))

murders %>%
  group_by(region) %>%
  summarise(tasa_100k_reg = sum(total) / sum(population) * 10^5) %>%
  ggplot(aes(x = region, y = tasa_100k_reg)) +
  geom_col()

murdersg |>
  ggplot() +
  geom_col(aes(x=grupo, y=tasa_100k))

murders |>
  ggplot()+
  geom_col(aes(x=tasa,y=region))

murders %>%
  ggplot() +
  geom_point(aes(x = population/10^6, y = total))

p<-murders %>%
  ggplot() +
  geom_point(aes(x = population/10^6, y = total),size=2)

p

p+
  geom_text(aes(population/10^6, total, label = abb))

p+
  geom_text(aes(population/10^6, total, label = abb),nudge_x = 1.5)

# Crear el gráfico base con puntos coloreados por región
p <- murders %>%
  ggplot() +
  geom_point(aes(x = population/10^6, y = total, colour = region), size = 2)

# Mostrar el gráfico
p

# Añadir etiquetas (con color por región)
p + 
  geom_text(aes(x = population/10^6, y = total, label = abb, colour = region))

# Añadir etiquetas desplazadas en el eje x (con color por región)
p<-p + 
  geom_text(aes(x = population/10^6, y = total, label = abb, colour = region), nudge_x = 1.5)
p

#démosle más información a la gráfica de dispersión.

p+labs(
  x = "Población (millones)",   # Cambia el nombre del eje x
  y = "Homicidios por arma de fuego",            # Cambia el nombre del eje y
  colour = "Región",              # Cambia el nombre de la leyenda de color
  title = "Homicidios por arma de fuego vs población, por región"  # Título del gráfico
)

#quizás esta forma no haya sido tan informativa... ¿qué tal si aplicamos una transformación logarítmica?
p2<-murders %>%
  ggplot() +
  geom_point(aes(x = log(population/10^6), y = log(total), colour = region), size = 3)
p2
p2+geom_text(aes(x = log(population/10^6), y = log(total), label = abb), nudge_y = 0.3)+
  labs(
  x = "Población (millones, escala log.)",   # Cambia el nombre del eje x
  y = "Homicidios por arma de fuego (escala log.)",            # Cambia el nombre del eje y
  colour = "Región",              # Cambia el nombre de la leyenda de color
  title = "Homicidios por arma de fuego vs población, por región"  # Título del gráfico
)

#de hecho siguiendo el libro, ggplot está acostumbrado a esta transformación y 
#ofrece sus funciones:
p3<-murders %>%
  ggplot() +
  geom_point(aes(x = population/10^6, y = total, colour = region), size = 3)
p3<- p3 + geom_text(aes(x = population/10^6, y = total, label = abb),nudge_x = 0.05)
p3<-p3+scale_x_continuous(trans = "log10")+
  scale_y_continuous(transform = "log10")
p3<-p3+
  labs(
    x = "Población (millones, escala log.)",   # Cambia el nombre del eje x
    y = "Homicidios por arma de fuego (escala log.)",            # Cambia el nombre del eje y
    colour = "Región",              # Cambia el nombre de la leyenda de color
    title = "Homicidios por arma de fuego vs población, por región"  # Título del gráfico
  )

library(ggthemes)
p3 + theme_fivethirtyeight()
#varios themes disponibles.

#todo junto

library(ggthemes)
library(ggrepel)

t <- murders |> 
  summarise(tasa = sum(total) /  sum(population) * 10^6) |>
  pull(tasa)

murders |> 
  ggplot(aes(population/10^6, total)) +   
  geom_abline(intercept = log10(t), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel(aes(label = abb)) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Homicidios por arma de fuego vs población, por región, 2010",
       x = "Población (millones, escala log.)", 
       y = "Homicidios por arma de fuego (escala log.)",
       color = "Región") +
  theme_economist()
