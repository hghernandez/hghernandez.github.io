library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#Cargo los datos del post que los obtuve de Kaggle
#URL https://www.kaggle.com/datasets/carrie1/ecommerce-data?resource=download


ruta <- paste0(getwd(),"/_posts/2022-09-05-retencion-de-clientes-analisis-por-cohortes")

df <- read.csv(paste0(ruta,"/data/data.csv"))

glimpse(df)

summarytools::st_options(lang = 'es')
summarytools::view(summarytools::dfSummary(df, plain.ascii  = FALSE, 
                                           style        = "grid", 
                                           graph.magnif = 0.75, 
                                           valid.col    = FALSE,
                                           tmp.img.dir  = "/tmp"))

#la fecha originalmente estaba en mdy y la deje en ymd

df <- df %>%
  mutate(Date= as_date(lubridate::mdy_hm(InvoiceDate)))

#Reviso cuantos tienen 12 meses y más clientes

View(df %>%
  group_by(Country) %>%
  summarise(min= min(Date),
            max= max(Date),
            clientes= n_distinct(CustomerID))%>%
  mutate(diferencia= interval(min,max) %/% months(1)))

#Selecciono UK por ahora 

#df <- dplyr::filter(df,Country== 'United Kingdom')

#Armamos el data frame con las cohortes

df <- df %>%
  filter(!is.na(CustomerID))%>%
  group_by(CustomerID)%>%
  mutate(min_fecha= min(Date),
         cohorte = paste0(month(min_fecha),"-",year(min_fecha)),
         mes= interval(min(Date),Date) %/% months(1))

View(df)

rate.retention <- df %>%
  group_by(cohorte,mes)%>%
  summarise(n = n_distinct(CustomerID))%>%
  mutate(rate= round(n*100/n[mes==0],1),
         cohorte= format(lubridate::my(cohorte),"%b-%Y")) %>%
  arrange(cohorte) %>%
  as.data.frame() %>%
  ggplot(aes(x= mes, y= reorder(cohorte,mes), fill= rate))+
  geom_tile()+
  geom_text(aes(label = rate), color = "white", size = 3) +
  scale_fill_gradient2(low = "#E0F3DB",mid="#A8DDB5",high = "#43A2CA") +
  scale_x_continuous(breaks = seq(0,12),expand = c(0,0))+
  labs(y= "Cohorte", x= "Mes", fill= "Tasa")+
  coord_fixed()+
  theme_minimal()

plotly::ggplotly(rate.retention,
                 tooltip = c("x","label"))


#Mean gasto
View(df)

monetize <- df %>%
  group_by(cohorte,mes)%>%
  summarise(prom= round(mean(UnitPrice),1))%>%
  mutate(cohorte= format(lubridate::my(cohorte),"%b-%Y")) %>%
  arrange(cohorte) %>%
  as.data.frame() %>%
  ggplot(aes(x= mes, y= reorder(cohorte,mes), fill= prom))+
  geom_tile()+
  geom_text(aes(label = prom), color = "white", size = 3) +
  scale_fill_gradient2(low = "#E0F3DB",mid="#A8DDB5",high = "#43A2CA") +
  scale_x_continuous(breaks = seq(0,12),expand = c(0,0))+
  labs(y= "Cohorte", x= "Mes", fill= "Tasa")+
  coord_fixed()+
  theme_minimal()

plotly::ggplotly(monetize,
                 tooltip = c("x","label"))



#Probamos los datos para los 3 mismos paises

paises <- c("United Kingdom","France","Germany")

rate.retention.country <- list()

for(pais in paises){
 
rate.retention.country[[pais]]<-
  df %>%
  filter(Country== pais)%>%
  group_by(cohorte,mes)%>%
  summarise(n = n_distinct(CustomerID))%>%
  mutate(rate= round(n*100/n[mes==0],1),
         cohorte= format(lubridate::my(cohorte),"%b-%Y")) %>%
  arrange(cohorte) %>%
  as.data.frame() %>%
  ggplot(aes(x= mes, y= reorder(cohorte,mes), fill= rate))+
  geom_tile()+
  geom_text(aes(label = rate), color = "white", size = 3) +
  scale_fill_gradient2(low = "#E0F3DB",mid="#A8DDB5",high = "#43A2CA") +
  scale_x_continuous(breaks = seq(0,12))+
  labs(y= "Cohorte", x= "Mes", fill= "Tasa")+
  coord_fixed()
  
  
}

plotly::ggplotly(rate.retention.country$`United Kingdom`,
                 tooltip = c("x","label"))

rate.retention.country[[1]]
