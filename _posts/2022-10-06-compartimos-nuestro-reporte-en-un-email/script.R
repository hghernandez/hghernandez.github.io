library(DBI)
library(odbc)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)
library(data.table)

#Cargo los datos del post que los obtuve de Kaggle
#URL https://www.kaggle.com/datasets/carrie1/ecommerce-data?resource=download


ruta <- paste0(getwd(),"/_posts/")

df <- read.csv(paste0(ruta,"/2022-10-02-envo-de-email-compartimos-nuestro-reporte/data/data.csv"))


#la fecha originalmente estaba en mdy y la deje en ymd

df <- df %>%
  mutate(Date= as_date(lubridate::mdy_hm(InvoiceDate)))%>%
  group_by(Country,Date,CustomerID)%>%
  summarise(Quantity= sum(Quantity),
            Monetary= sum(UnitPrice)) %>%
  filter(Country %in% c("United Kingdom")) %>%
  filter_if(is.numeric, ~ .x > 0)




source(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/Modulos/normalize_table.R"),encoding = "UTF-8")


df <- normalize_table(df = df,
                date = "Date",
                id_costumer = "CustomerID",
                cantidad = "Quantity",
                monto = "Monetary")


source(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/Modulos/rfm_category.R"),encoding = "UTF-8")



#Armo el archivo

tabla_rfm <- df %>%
  mutate(email= "cliente@rfm.com")%>%
  split(.$Country) %>%
  map(~ rfm_category(df = .,
                     fecha_analisis = '2011-12-09',
                     bins = 4,
                     group_by = "Country",
                     inherits.threshold = NULL))


DT::datatable(tabla_rfm[[1]]$resultado_rfm %>% head(10),filter= 'top',options = list(pageLength = 5, dom = 'tip'))



#Cargo la función de segmentación

source(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/Modulos/segment_rfm.R"),encoding = "UTF-8")


#Creo los nombres de los segmentos y los puntos de corte

nombres_segmentos <- c("Champions","Loyalist","Big Spenders",
                       "Promising","New Customers","Hibernating")
recency_lower <-   c(4,1,1,2,4,1)
recency_upper <-   c(4,4,4,4,4,1)
frequency_lower <- c(4,4,1,2,1,1)
frequency_upper <- c(4,4,4,4,4,1)
monetary_lower <-  c(4,1,4,2,1,1)
monetary_upper <-  c(4,4,4,4,4,1)


#Armo el archivo


rfm_uk <- segment_rfm(tabla_rfm = tabla_rfm[[1]],
                          nombres_segmentos = nombres_segmentos,
                          recency_lower,
                          recency_upper,
                          frequency_lower,
                          frequency_upper,
                          monetary_lower,
                          monetary_upper
  )


rfm_uk$impact_segment$Ventas

scales::comma(rfm_uk$impact_segment$Ventas[1],big.mark=".", decimal.mark=",")

max(rfm_uk$Composicion_segmento$`%`) 

format(lubridate::ymd(Sys.Date()),"%B %Y")

rfm_uk$impact_segment$segmento[1]
rfm_uk$impact_segment[1,3]


scales::comma(rfm_uk$impact_segment$Monto[1],big.mark=".",decimal.mark = ",")

porc <- rfm_uk$impact_segment$
scales::comma(porc,big.mark=".",decimal.mark = ",",digits = 1)
