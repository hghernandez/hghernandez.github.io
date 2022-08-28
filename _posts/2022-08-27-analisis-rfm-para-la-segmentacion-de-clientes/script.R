library(DBI)
library(odbc)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)
library(data.table)

#Cargo los datos Geopagos

ruta <- paste0(getwd(),"/_posts/")

load(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/data_test/UT_rfm.Rdata"))


#Cargo los datos del post que los obtuve de Kaggle
#URL https://www.kaggle.com/datasets/carrie1/ecommerce-data?resource=download


ruta <- paste0(getwd(),"/_posts/")

df <- read.csv(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/data/data.csv"))

summarytools::st_options(lang = 'es')
summarytools::view(summarytools::dfSummary(df, plain.ascii  = FALSE, 
                                           style        = "grid", 
                                           graph.magnif = 0.75, 
                                           valid.col    = FALSE,
                                           tmp.img.dir  = "/tmp",
                                           c))

#la fecha originalmente estaba en mdy y la deje en ymd

df <- df %>%
  mutate(Date= as_date(lubridate::mdy_hm(InvoiceDate)))%>%
  group_by(Country,Date,CustomerID)%>%
  summarise(Quantity= sum(Quantity),
            Monetary= sum(UnitPrice)) %>%
  filter(Country %in% c("United Kingdom","Germany","France")) %>%
  filter_if(is.numeric, ~ .x > 0)




#Determino los paises con mas clientes y las fechas

View(df %>%
       group_by(Country) %>%
       summarise(min= min(Date),
                 max= max(Date),
               clientes= n_distinct(CustomerID)))

source(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/Modulos/normalize_table.R"),encoding = "UTF-8")


df <- normalize_table(df = df,
                date = "Date",
                id_costumer = "CustomerID",
                cantidad = "Quantity",
                monto = "Monetary")

DT::datatable(df,filter= 'top',options = list(pageLength = 5, dom = 'tip'))

#Cargo la funcion de categorizacion

source(paste0(ruta,"/2022-08-27-analisis-rfm-para-la-segmentacion-de-clientes/Modulos/rfm_category.R"),encoding = "UTF-8")

#Armo el archivo

tabla_rfm <- df %>%
  mutate(email= "cliente@rfm.com")%>%
  split(.$Country) %>%
  map(~ rfm_category(df = .,
                     fecha_analisis = '2011-12-09',
                     bins = 4,
                     group_by = "Country"))


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


rfm <- list()

for(i in 1:length(tabla_rfm)){
  rfm[[i]] <- segment_rfm(tabla_rfm = tabla_rfm[[i]],
                          nombres_segmentos = nombres_segmentos,
                          recency_lower,
                          recency_upper,
                          frequency_lower,
                          frequency_upper,
                          monetary_lower,
                          monetary_upper
  )
}


rfm[[1]]$tabla_rfm$Country

rfm[[1]]$tabla_rfm
rfm[[1]]$bar_chart
rfm[[3]]$Composicion_segmento
rfm[[1]]$bar_chart_seg
rfm[[3]]$impact_segment

View(rfm[[3]]$tabla_rfm %>%
       filter(segmento=='Otros')%>%
       group_by(recency_cut,frequency_cut,monetary_cut)%>%
       summarise(n= n_distinct(user_id))%>%
       arrange(desc(n)))

print(kableExtra::kbl(rfm[[3]]$impact_segment,
        col.names = c("Segmentos","Ventas","%","Monto","%"),
                      
        format.args = list(decimal.mark = ',', big.mark = ".")) %>%
        
        kableExtra::kable_paper())


