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
#ejemplos adicionales, y el quickplot
data(murders)
x <- log10(murders$population)
y <- murders$total

data.frame(x = x, y = y) |>
  ggplot(aes(x, y)) +
  geom_point()

qplot(x, y)
murdersg<-murders %>%
  mutate(tasa= total/population *10^5,
    ,grupo = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Nueva Inglaterra",
    abb %in% c("WA", "OR", "CA") ~ "Costa del Pacífico",
    region == "South" ~ "el Sur",
    TRUE ~ "Otras regiones"))
qplot(tasa, data = murdersg, geom= "density", fill = grupo, linetype=grupo)

a <- murdersg|>
  ggplot(aes(x = tasa))
a 
a+geom_area(stat="bin")
a+ geom_density()
a+ geom_density(aes(color = grupo)) 
a+ geom_density(aes(fill = grupo),alpha=0.4) 


mu<-murdersg %>%
  group_by(grupo) %>%
  summarise(mediagrupal=mean(tasa))
head(mu)

a+ geom_density(aes(color = grupo)) +
  geom_vline(data=mu, aes(xintercept=mediagrupal, color=grupo),
             linetype="dashed") +
  scale_color_manual(values=c("#999999", "#E69F00","skyblue","red3")) 
 
data(heights)            
mu_alt<-heights %>%
  group_by(sex) %>%
  summarise(media=mean(height))
b<-heights|>ggplot()
qplot(sex, height, data = heights, geom= "boxplot", fill = sex)
qplot(sex, height, data = heights, geom= "violin", fill = sex)
qplot(sex, height, data = heights, geom = "dotplot",
      stackdir = "center", binaxis = "y", dotsize = 0.3)
qplot(height, data = heights, geom = "density", fill = sex)
qplot(height, data = heights, geom = "density", color = sex, linetype = sex)
b
b+geom_density(aes(x=height,colour = sex)) 
b+geom_density(aes(x=height,fill = sex), alpha=0.4)
mu_alt
c<-heights|>ggplot(aes(x = height))
c+geom_density(aes(fill = sex), alpha=0.4)
c+ geom_density(aes(color = sex)) +
  geom_vline(data=mu_alt, aes(xintercept=media, color=sex),
             linetype="dashed") +
  scale_color_manual(values=c("#999999", "#E69F00")) 


#Visualización en la práctica
library(tidyverse)
library(dslabs)
data(gapminder)
gapminder |> as_tibble()

gapminder |>
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) |>
  select(country, infant_mortality)
#¿se divide el mundo en las dicotomías de países ricos y occidentales ricos y de alta esperanza de vida
#vs países pobres de familias grandes y vidas cortas?
#¡usemos datos y visualizaciones!
filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

filter(gapminder, year == 1962) |>
  ggplot(aes(fertility, life_expectancy, colour = continent)) +
  geom_point()

filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, colour = continent)) +
  geom_point() +
  facet_grid(continent~year)

filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(. ~ year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder |>
  filter(year %in% years & continent %in% continents) |>
  ggplot( aes(fertility, life_expectancy, colour = continent)) +
  geom_point() +
  facet_wrap(~year)

filter(gapminder, year%in%c(1962, 2012)) |>
  ggplot(aes(fertility, life_expectancy, colour = continent)) +
  geom_point() +
  facet_wrap(. ~ year, scales = "free")

gapminder |>
  filter(country == "United States") |>
  ggplot(aes(year, fertility)) +
  geom_point()

gapminder |>
  filter(country == "United States") |>
  ggplot(aes(year, fertility)) +
  geom_line()

countries <- c("South Korea","Germany")

gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year, fertility, group = country)) +
  geom_line()

gapminder |> filter(country %in% countries & !is.na(fertility)) |>
  ggplot(aes(year,fertility, colour = country)) +
  geom_line()

labels <- data.frame(country = countries, x = c(1975,1965), y = c(60,72))

gapminder |>
  filter(country %in% countries) |>
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")


gapminder <- gapminder |> mutate(dollars_per_day = gdp/population/365)

past_year <- 1970



gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  mutate(region = reorder(region, dollars_per_day, FUN = median)) |>
  ggplot(aes(dollars_per_day, region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

gapminder <- gapminder |>
  mutate(group = case_when(
    region %in% c("Western Europe", "Northern Europe","Southern Europe",
                  "Northern America",
                  "Australia and New Zealand") ~ "Occidente",
    region %in% c("Eastern Asia", "South-Eastern Asia") ~ "Asia oriental",
    region %in% c("Caribbean", "Centroamérica y Caribe",
                  "South America") ~ "Sudamérica",
    continent == "Africa" &
      region != "Northern Africa" ~ "África subsahariana",
    TRUE ~ "Otros"))
#le damos orden a los niveles
gapminder <- gapminder |>
  mutate(group = factor(group, levels = c("Otros", "Sudamérica",
                                          "Asia oriental", "África subsahariana",
                                          "Occidente")))

p <- gapminder |>
  filter(year == past_year & !is.na(gdp)) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
p + geom_point(alpha = 0.5)

library(ggridges)
p <- gapminder |>
  filter(year == past_year & !is.na(dollars_per_day)) |>
  ggplot(aes(dollars_per_day, group)) +
  scale_x_continuous(trans = "log2")
p + geom_density_ridges()
p + geom_density_ridges(jittered_points = TRUE)

p + geom_density_ridges(jittered_points = TRUE,
                        position = position_points_jitter(height = 0),
                        point_shape = '|', point_size = 3,
                        point_alpha = 1, alpha = 0.7)
gapminder$group

past_year <- 1970
present_year <- 2010
years <- c(past_year, present_year)
gapminder |>
  filter(year %in% years & !is.na(gdp)) |>
  mutate(west = ifelse(group == "Occidente", "Occidente", "El Resto")) |>
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ west)

country_list_1 <- gapminder |>
  filter(year == past_year & !is.na(dollars_per_day)) |>
  pull(country)

country_list_2 <- gapminder |>
  filter(year == present_year & !is.na(dollars_per_day)) |>
  pull(country)

country_list <- intersect(country_list_1, country_list_2)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(group, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("") +
  facet_grid(. ~ year)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(year = factor(year)) |>
  ggplot(aes(group, dollars_per_day, fill = year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = "log2") +
  xlab("")

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  ggplot(aes(dollars_per_day)) +
  geom_density(fill = "grey") +
  scale_x_continuous(trans = "log2") +
  facet_grid(. ~ year)

gapminder |>
  filter(year %in% years & country %in% country_list) |>
  mutate(group = ifelse(group == "Occidente", "Occidente", "Resto")) |>
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2) +
  facet_grid(year ~ .)
