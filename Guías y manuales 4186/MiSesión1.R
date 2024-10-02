library(dslabs) # cargando paquete de libro de curso
data(murders)
class(murders)
str(murders)
head(murders)
murders$population #hasta aquí estábamos trabajando con observar el objeto
names(murders)
class(murders$region)
levels(murders$region)
summary(murders)
View(murders) #aquí vimos el objeto casi como una tabla de Excel
sort(murders$total)
ordenado<-order(murders$total)
murders$state[ordenado]

# Manipulación sencilla de datos ------------------------------------------

max(murders$total)
i_max<-which.max(murders$total)
murders$state[i_max]
View(murders)
murders$tasa_asesinato<-murders$total/murders$population*100000
murders$state[order(murders$tasa_asesinato)]
View(murders)

# Visualización básica: plot e hist

x=murders$population/10^6
y=murders$total
plot(x,y)
with(murders,plot(population,total))

plot(murders$population,murders$total)
with(murders, hist(tasa_asesinato))
boxplot(tasa_asesinato~region,data = murders)
