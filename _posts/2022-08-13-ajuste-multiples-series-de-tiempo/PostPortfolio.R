if(!require("modeltime")) install.packages("modeltime")
if(!require("rsample")) install.packages("rsample")
if(!require("parsnip")) install.packages("parsnip")
if(!require("recipes")) install.packages("recipes")
if(!require("workflows")) install.packages("workflows")
if(!require("dplyr")) install.packages("dplyr")
if(!require("tidyr")) install.packages("tidyr")
if(!require("sknifedatar")) install.packages("sknifedatar")
if(!require("earth")) install.packages("earth")
if(!require("glmnet")) install.packages("glmnet")
if(!require("DBI")) install.packages("DBI")
if(!require("kableExtra")) install.packages("kableExtra")
if(!require("gt")) install.packages("gt")
if(!require("googlesheets4")) install.packages("googlesheets4")
if(!require("googledrive")) install.packages("googledrive")

#Activa el package
library(modeltime)
library(rsample)
library(parsnip)
library(recipes)
library(workflows)
library(dplyr)
library(tidyr)
library(sknifedatar)
library(timetk)
library(lubridate)
library(googlesheets4)
library(httr)

ruta <- here::here("_posts/2022-08-13-ajuste-multiples-series-de-tiempo")

source(paste0(ruta,"/Modulos/view_times_series.R"),encoding = "UTF-8")

#Traigo el indice de precio al consumidor nivel general por region

obj = GET("https://apis.datos.gob.ar/series/api/series?ids=145.3_INGCUYUAL_DICI_M_34,145.3_INGNEAUAL_DICI_M_33,145.3_INGNOAUAL_DICI_M_33,145.3_INGPATUAL_DICI_M_39,145.3_INGGBAUAL_DICI_M_33,145.3_INGNACUAL_DICI_M_38,145.3_INGPAMUAL_DICI_M_38&format=csv")

content <- httr::content(obj,encoding = "UTF-8") 

data <- data.frame("Mes"= content$indice_tiempo,
      "GBA"= content$ipc_ng_gba_tasa_variacion_mensual,
      "Cuyo"= content$ipc_ng_cuyo_tasa_variacion_mensual,
      "NEA"=content$ipc_ng_nea_tasa_variacion_mensual,
      "NOA"= content$ipc_ng_noa_tasa_variacion_mensual,
      "Pampeana"= content$ipc_ng_pampeana_tasa_variacion_mensual,
      "Patagonia"= content$ipc_ng_patagonia_tasa_variacion_mensual,
      "Nacional"= content$ipc_ng_nacional_tasa_variacion_mensual) %>%
  pivot_longer(!Mes,names_to = "Region", values_to = "IPC") %>%
  mutate(IPC = round(IPC * 100,1)) %>%
  rename("date"= "Mes", "value"= "IPC")


#Observemos las series de tiempo

view_times_series(df = data,
                  x = "date",
                  y = "value",
                  facet = "Region",
                  title = "IPC Nivel General (base 2016) por región",
                  y_lab = "IPC",
                  scales = "free_y"
)


############################################################
#                                                          #
#                     Preprocesamiento                     #
#                                                          #
############################################################

#Anido las series de tiempo

ipc_nested = data  %>% nest(nested_column=-Region) 
ipc_nested

#Armo las recetas

#Esta receta la utiliza el modelo de Redes Neuronales

receta_IPC_1 = recipe(value ~ ., data = data %>% select(-Region)) %>%
  step_date(date, features = c("month", "quarter", "year"), ordinal = TRUE)


#Esta receta es para el resto de los modelos

receta_IPC_2 = receta_IPC_1  %>%
  step_mutate(date_num = as.numeric(date)) %>% 
  step_normalize(date_num) %>%
  step_rm(date) %>% 
  step_dummy(date_month)



##Instanciacion de los modelos

m_arima_boosted_ipc <- workflow() %>% 
  add_recipe(receta_IPC_1) %>% 
  add_model(arima_boost() %>% set_engine(engine = "auto_arima_xgboost"))

m_seasonal_ipc <- seasonal_reg() %>%
  set_engine("stlm_arima")

m_prophet_boost_ipc <- workflow() %>% 
  add_recipe(receta_IPC_1) %>% 
  add_model(prophet_boost(mode='regression') %>%set_engine("prophet_xgboost")) 

m_nnetar_ipc <- workflow() %>%
  add_recipe(receta_IPC_1) %>%
  add_model(nnetar_reg() %>% set_engine("nnetar"))

m_mars_ipc <- workflow() %>%
  add_recipe(receta_IPC_2) %>%
  add_model(mars(mode = "regression") %>% set_engine("earth"))

m_glmnet_ipc <- workflow() %>%
  add_recipe(receta_IPC_2) %>%
  add_model(linear_reg(penalty = 0.01, mixture = 0.1) %>% set_engine("glmnet"))


m_xgboost_ipc <- workflow() %>%
  add_recipe(receta_IPC_2) %>%
  add_model(boost_tree() %>% set_engine("xgboost"))

#División train y test


split <- 0.75

model_table_ipc <- modeltime_multifit(
  serie = ipc_nested,
  .prop = split,
  m_arima_boosted_ipc,
  m_seasonal_ipc,
  m_prophet_boost_ipc,
  m_nnetar_ipc,
  m_mars_ipc,
  m_glmnet_ipc,
  m_xgboost_ipc)



#Creo una tabla del mejor modelo con el RMSE

model_accuracy <- model_table_ipc$models_accuracy %>%
  group_by(name_serie) %>%
  mutate(rmse_min= min(rmse))%>% #Lo uso para seleccionar el mejor modelo
  filter(rmse== rmse_min)%>%
  select(-rmse_min) %>%
  rename_with(., ~ gsub(".", "", .x, fixed = TRUE))

View(model_accuracy)

write.table(model_table_ut_geo$models_accuracy %>%
  mutate(across(where(is.numeric), round, 2)),"clipboard",dec=",",
  sep="\t",row.names = F)



############################################################
#                                                          #
#                Selección del mejor modelo                #
#                                                          #
############################################################



best_model_ipc <- modeltime_multibestmodel(
  .table = model_table_ipc$table_time,
  .metric = "rmse",
  .minimize = TRUE,
  .forecast = FALSE
  
)



#Acá estan las predicciones con el mejor modelo de la serie de tiempo
best_model_ut_geo$calibration[[1]]$.calibration_data
#Acá esta el nombre del mejor modelo
best_model_ut_geo$calibration[[1]]$.model_desc


#Armo una tabla con el mejor modelo por partner

best.model <- cbind(Region= best_model_ipc$Region[1],Modelo=best_model_ipc$calibration[[1]]$.model_desc)

for(i in 2:length(best_model_ut_geo$calibration)){

  best.model <- rbind(best.model,
                      cbind(Region=best_model_ipc$Region[i],Modelo=best_model_ipc$calibration[[i]]$.model_desc))
  
}

best.model <-as.data.frame(best.model)

View(best.model)
write.table(best.model,"clipboard",sep="\t", row.names = F, dec= ",")

############################################################
#                                                          #
#         Entreno el mejor modelo para cada serie          #
#                                                          #
############################################################


model_refit_ipc <- modeltime_multirefit(best_model_ipc)


#Uno los datos con la tabla original

model_refit_ipc <- model_refit_ipc %>% 
  bind_cols(data %>% 
              nest(nested_column=-Region) %>% 
              select(-Region) %>% 
              rename(actual_data=nested_column)
  )


model_refit_ipc$actual_data


############################################################
#                                                          #
#         Realizo el forecast con el mejor modelo          #
#                                                          #
############################################################


forecast_ipc <- modeltime_multiforecast(models_table = model_refit_ipc,
                                           .h = 12,
                                           .prop = split)

forecast <- cbind(Region= forecast_ipc$Region[1],forecast_ipc$nested_forecast[[1]])


for(i in 2:length(forecast_ipc$nested_forecast)){
  
  forecast <- rbind(forecast,
                    cbind(Region= forecast_ipc$Region[i],forecast_ipc$nested_forecast[[i]]))
}


forecast_ipc <- forecast %>%
  rename_with(.,~  gsub(".","",.x, fixed= TRUE)) %>% #Quito los puntos de los nombres
  rename(Mes = index, IPC= value)

is.na(forecast_ipc)<-sapply(forecast_ipc, is.infinite)
is.na(forecast_ipc)<-sapply(forecast_ipc, is.nan)
options(scipen=1000000)



write.table(forecast,'clipboard-60000',
            sep="\t",row.names = F, dec= ",")


write.table(forecast,'clipboard-60000',
            sep="\t",row.names = F, dec= ",")

save(proyecciones, file= "Evaluacion/Reporte DEV/proyecciones.RData")

source("Evaluacion/Modulos/plot_time_series.R")

grafico <- forecast_ipc %>%
  plot_time_series(df = .,title = "Forecast IPC",
                   y_lab = "IPC mensual",
                   x = "Mes",
                   y = "IPC",
                   facet = "Region")
grafico
ggsave(grafico,height = 25, width = 25, units = "cm",
       filename ="Evaluacion/forecast_ut_models.png")




