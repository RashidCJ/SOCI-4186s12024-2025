library(tidyverse)
library(dslabs)
data("murders")
#añadir una columna con mutación (mutate)
murders<-mutate(murders,tasa=total/population*100000)
#alternativamente podríamos crear otra variable
murders <- mutate(murders, pob_millones = population/ 10^6)
#noten que no hay que escribir '$' porque la fórmula mutate 
#sabe que estamos trabajando con columnas del objeto murders.
head(murders)
#creando subconjuntos con filtros (filter)
filter(murders,tasa<=0.9) #cambia la tasa para que vean
filter(murders, state=="Alabama") #Roll Tide
filter(murders, state=="Indiana") #Go Hoosiers
sin_florida <- filter(murders, state != "Florida")
sin_sur  <- filter(murders, region != "South")
filter(murders,population > 5000000 & region =="Northeast")
#seleccionando columnas con (select)
tabla_nueva<- select(murders, state, region, tasa)
filter(tabla_nueva,tasa<=0.71)
#El pipe %>% o |> (desde una versión de R ambas se pueden)
# es útil para una serie de operaciones, enviando los resultados
# de una función a la siguiente. e.g.
murders %>% select(state,region, tasa) |> filter(tasa<=0.71)
#ejemplo más intuitivo
81 |> sqrt() |> sqrt()
select(murders, state, population) %>% 
  head()

#Iniciando el análisis explorativo: Resumiendo datos.
#summarise o summarize, datos de alturas
data("heights")
resumen<-heights|>
  filter(sex=="Female") |>
  summarise(media_altura=mean(height),desviación_estándar=sd(height))
resumen
resumen_m<-heights|>
  filter(sex=="Male") |>
  summarise(media_altura=mean(height),desviación_estándar=sd(height))
resumen_m
#boxplot(height~sex,heights)
summarise(murders,mean(tasa))
#tasa promedio sin ponderar correctamente
tasa_homicidios_armas_de_fuego_EEUU<-murders |>
  summarise(tasa=sum(total)/sum(population)*100000)
tasa_homicidios_armas_de_fuego_EEUU

#si lo que queríamos era ver los máximos
# Seleccionar la fila con el valor máximo en la columna `tasa`
max_row <- murders |>
  #mutate(tasa = total / population * 10^5) |> #comentado para invisibilizarlo, esto ya estaba
  slice_max(tasa) # Selecciona la fila con el máximo `rate`

max_row

# Filtrar la fila donde el `tasa` es mínimo
min_row <- murders |>
  #mutate(tasa = total / population * 10^5) |> #comentado para invisibilizarlo, esto ya estaba
  filter(row_number() == which.min(tasa))

min_row

#Resumen múltiple: queremos mediana, mínimo, máximo:
heights|>
  filter(sex=="Female") |>
  summarise(mediana=median(height), mínima=min(height),
            máxima=max(height))
#ó
heights |>
  filter(sex == "Female") |>
  reframe(mediana_mín_máx = quantile(height, c(0.5, 0, 1)))
#el último es un cambio de dplyr reciente, summarise funciona 
# igual heights|> filter(sex=="Female")|> summarise(mediana_mín_máx=quantile(height,c(0.5,0,1)))
mediana_mín_máx <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(mediana = qs[1], mín = qs[2], máx = qs[3])
}

heights %>%
  filter(sex == "Female") %>%
  summarize(mediana_mín_máx(height))

#agrupando: al explorar datos quizás querramos ver los grupos y cómo
# se distribuye la información a través destos.
heights %>%
  group_by(sex)

library(dplyr)


heights %>%
  group_by(sex) %>%
  summarise(media=mean(height), desviación_estándar=sd(height))

murders |> group_by(region) |> summarise(mediana_mín_máx(tasa))

#ordenando data frames
murders |> arrange(population) |> head() #Por defecto, ascendente
murders |> arrange(tasa) |> head() 
murders |> arrange(desc(population)) |> head() #descendente
murders |> arrange(desc(tasa)) |> head()
#anidadamente
murders |> arrange(region,desc(tasa)) |> head()
murders |> top_n(5,tasa)

tasas<-filter(murders,region=="South")|>
  pull(tasa)
median(tasas)

murders %>%
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Nueva Inglaterra",
    abb %in% c("WA", "OR", "CA") ~ "Costa del Pacífico",
    region == "South" ~ "el Sur",
    TRUE ~ "Otras regiones")) %>%
  group_by(group) %>%
  summarise(rate = sum(total)/ sum(population) * 10^5)
