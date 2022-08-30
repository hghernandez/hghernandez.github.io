library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(leaflet.extras2)
library(osrm)

#Guardamos la URL de geoservicios

geo_gchu <- "https://geo.gualeguaychu.gov.ar/geoserver/gis/wms?Request=GetCapabilities"


#Exploramos las capas

layers <- st_layers("WFS:https://geo.gualeguaychu.gov.ar/geoserver/gis/wms?Request=GetCapabilities")

head(layers$name,10)

#Traemos algunas capas 

#veamos las areas programáticas en un mapa y los establecimientos de salud

#Muy interesante la funcion addWMS de leaflet.extras2 muestra metadata

leaflet() %>%
  addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png",
           tileOptions(tms = TRUE,maxZoom = 14), attribution = '<a target="_blank" href="https://www.ign.gob.ar/argenmap/argenmap.jquery/docs/#datosvectoriales" style="color: black; text-decoration: underline; font-weight: normal;">Datos IGN Argentina // OpenStreetMap</a>',
           group = "Argenmap") %>%
  addWMS(
    geo_gchu,
    layers = "radios_censales",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,info_format = "text/html", tiled=FALSE),
    group = "Radios Censales"
  ) %>%
  addWMS(
    geo_gchu,
    layers = "areas_programaticas",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,info_format = "text/html", tiled=FALSE),
    group = "Areas Programáticas"
  )  %>%
  addWMS(
    geo_gchu,
    layers = "est_salud",
    options = WMSTileOptions(format = "image/png", transparent = TRUE,info_format = "text/html", tiled=FALSE),
    group = "Est de salud"
  ) %>%
  
  addCircles(data= centroides,group = "centroides",color = "black")%>%
  addLayersControl(
    baseGroups = "Argenmap",
    overlayGroups = c("Areas Programáticas","Est de salud","Radios Censales","centroides"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lng = -58.52597, lat = -33.00606,zoom = 11)

  
#Extraemos las capas de interés

#Extraemos las areas programaticas
areas_programaticas <- st_read("WFS:https://geo.gualeguaychu.gov.ar/geoserver/gis/wms?Request=GetCapabilities",
                               "gis:areas_programaticas")

#Extraemos los establecimientos de salud

est_salud <- st_read("WFS:https://geo.gualeguaychu.gov.ar/geoserver/gis/wms?Request=GetCapabilities",
                     "gis:est_salud")

#Extraemos los radios censales

radios_censales <- st_read("WFS:https://geo.gualeguaychu.gov.ar/geoserver/gis/wms?Request=GetCapabilities",
                           "gis:radios_censales")

radios_censales <- radios_censales %>% 
  filter(!gml_id %in% c("radios_censales.2","radios_censales.11"))
  
View(radios_censales)
##%######################################################%##
#                                                          #
####                     Ejercicio                      ####
#                                                          #
##%######################################################%##

#Estimar la distancia desde cada centroide de radio censal dentro de 
#cada area programatica hasta centro de salud
#Estimar distancia a pie

#homogeneizamos las proyecciones

csalud <- est_salud %>% st_drop_geometry()

csalud <- st_as_sf(csalud[c("fid","nombre","longitud","latitud")], coords = c("longitud", "latitud"), 
         crs = 4326, remove= FALSE) %>% st_transform(crs = 4326)


est_salud <- st_transform(est_salud, 4326)

areas_programaticas <- st_transform(areas_programaticas, 4326)

areas_programaticas <- st_cast(areas_programaticas, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

join <- st_join(areas_programaticas,csalud,join= st_intersects)

DT::datatable(join %>% st_drop_geometry() %>% head(5))

#Armo los centroides de los radios censales

radios_censales <- st_transform(radios_censales, 4326)

radios_censales <- st_cast(radios_censales, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")


radios_censales$centroides <- radios_censales %>%  st_centroid() %>%
  st_geometry() 




centroides <- radios_censales %>%
  select(gml_id)%>%
  st_centroid() %>%
  st_as_sf()


join2 <- st_join(join,radios_censales,join= st_intersects)


DT::datatable(join2 %>% st_drop_geometry() %>% head(5),
              options = list(scrollx=TRUE))
#Armo un dataframe de distancias

distancias <- st_join(est_salud %>%
       select("fid","nombre")%>%
       rename("geomcs"="geom"),join2 %>%
         select("gml_id.y","centroides"), join= st_intersects) %>%
  distinct()

#separo las geometrias en dos archivos

from <- distancias %>%
  select(gml_id.y,centroides)%>%
  st_drop_geometry(geomcs) %>%
  st_as_sf()

to <- distancias %>%
  select(fid,nombre,geomcs)
  
#Cargo todo a una lista

rutas <- list()

for(i in 1:nrow(from)){

print(i)  
  
rutas[[i]] <- osrmRoute(src = from[i,],
          dst = to[i,],
          overview = FALSE)
}

df <- do.call(rbind,rutas)

distancias_x_cs <- cbind(distancias[,c(1,2)] %>% st_drop_geometry(),df)

join %>%
  st_drop_geometry()%>%
  select(gml_id,fid)%>%
  left_join(distancias_x_cs, by= "fid") %>%
  DT::datatable(options = list(pageLength = 25, dom = 'tip'))

# Metricas

join %>%
  st_drop_geometry()%>%
  select(gml_id,fid)%>%
  left_join(distancias_x_cs, by= "fid") %>%
  group_by(gml_id)%>%
  filter(!is.na(duration)) %>%
  summarise(min= round(min(duration),1),
            prom= round(mean(duration),1),
            med= round(median(duration),1),
            max= round(max(duration),1)) %>%
  arrange(desc(prom))%>%
  kableExtra::kbl(format.args = list(big.mark = ".",
                                     decimal.mark= ",")) %>%
  kableExtra::kable_classic_2()

#El radio censal 13 cae dentro de 3 areas programaticas
# Villa Maria
# Suburbio Sur
# Médanos

# tiene la posibilidad de ir al Hospital, Al Baggio, al Villa Maria o
# al suburbio sur



#Muestreo 5 puntos al azar

radios_censales_mod <-  st_cast(radios_censales, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

p1 = st_sample(radios_censales_mod,c(3,10), exact = TRUE, type = "random")

plot(st_geometry(radios_censales_mod))
plot(p1, add = TRUE)
  
cbind(radios_censales_mod[1:3, ],p1)

htmltools::tag("aside","hola mundo")
