library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)

#Cargo el dataset

ruta <- paste0(getwd(),"/_posts/")

load(paste0(ruta,"2022-07-30-analisis-de-estacionalidad/dataset_estac.RData"))

#Modifico los nombres de los partners


data <- data %>%
  mutate(Fecha = lubridate::as_date(Date)) %>%
  group_by(Fecha,Merchant)%>%
  summarise(Cant_Ventas= sum(Cant_Ventas))

#Guardo el dataset

save(data,file= paste0(ruta,"2022-07-30-analisis-de-estacionalidad/dataset_estac.RData"))



#Armo el analisis de estacionalidad

#Cargo las funciones

source(paste0(ruta,"2022-07-30-analisis-de-estacionalidad/Modulos/create_seasonal.R"),encoding = "UTF-8")


estacionalidad <- data %>%
  mutate(nombre= Merchant) %>%
  nest(column_nest= -c(Merchant)) %>%
  mutate(seasonal_dx = map(column_nest, ~ create.seasonal(df = .,
                                                          date = "Fecha")))


estacionalidad$seasonal_dx[[1]]$grafico

length(estacionalidad$seasonal_dx)
#Guardo la lista de estacionalidad

save(estacionalidad,file= "C:/Users/usuario/Documents/hghernandez.github.io/_posts/2022-07-30-analisis-de-estacionalidad/estacionalidad.RData")

load("C:/Users/usuario/Documents/Portfolio/_posts/2022-07-30-analisis-de-estacionalidad/dataset_estac.RData")

estacionalidad$seasonal_dx[[i]]$grafico

##%######################################################%##
#                                                          #
####               Armo el mapa de calor                ####
#                                                          #
##%######################################################%##

#Cargo la funcion


source(paste0(ruta,"2022-07-30-analisis-de-estacionalidad/Modulos/heatmap_calendar.R"),encoding = "UTF-8")

max(data$Fecha)

heatmap_calendar <- data %>%
  mutate(nombre= Merchant) %>%
  nest(column_nest= -c(Merchant)) %>%
  mutate(heatmap_calendar = map(column_nest,~heatmap.calendar(df = .,
                                                              fini = '01-06-2021',
                                                              ffin = '31-12-2021',
                                                              ColorBrewer = "GnBu")))

heatmap_calendar$heatmap_calendar[[2]]

heatmap_calendar$Merchant

length(heatmap_calendar$heatmap_calendar)

#Guardo los calendarios

save(heatmap_calendar,file= "C:/Users/usuario/Documents/hghernandez.github.io/_posts/2022-07-30-analisis-de-estacionalidad/heat_map.RData")

devtools::install_github("hadley/emo")

emo::ji("open_hands_medium_light_skin_tone")

