rm(list = ls())

#Aqui leo lalibreria y cargo la base de datos
# agregar

library(tsibble)
library(dplyr)
base = readxl::read_excel("C:/Users/Mario/Desktop/Ramos U/Optativos/Analisis predictivo/Base de datos/tourism.xlsx")

#Observo el tsibble tourism de la libreria tsibble, para ver como debe quedar
View(tourism)

#Se crea un tibble identico al tourism visto anteriormente modific<r
identico = base %>% dplyr::mutate(Quarter = yearquarter(Quarter))
#Lo que cambio con el que importamos es la columna Quarter

#Lo transformo en tsibble
identico = identico %>% as_tsibble( index=Quarter, key=c(Region,State,Purpose))

#Filtro dentro de la columna de Trips la fila que tiene el mayor valor de Trips
#Y luego selecciono que me muestre las columnas region y proposito
identico %>% filter(Trips==max(Trips)) %>% select(Region,Purpose)-> identico2

#Para aruparlos segun una categoria y luego aplicar la estadistica y sumar
identico %>%group_by(State) %>%  summarise(total=sum(Trips))



