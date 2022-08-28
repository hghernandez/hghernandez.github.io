
rfm_category <- function(df,fecha_analisis,bins, group_by){
  
  
  
  #Preprocesamiento
  df <- df %>% dplyr::ungroup()
  
  df <- dplyr::mutate(df, date = as.Date(date),
                      email= dplyr::case_when(grepl("@",email)== FALSE ~ NA_character_,
                                              TRUE ~ email))
  df <- dplyr::mutate(df,user_id= dplyr::group_indices(df,!!rlang::sym(group_by),user_id))
  df <- dplyr::filter(df,user_id != 0 & !grepl('@geopagos',email))
  df <- dplyr::group_by(df,!!rlang::sym(group_by),user_id)
  df <- dplyr::summarise(df,
                         recency= as.numeric(as.Date(fecha_analisis)-max(date)),
                         frequency= sum(cantidad),
                         monetary = sum(monto))
  
  #genero los quantile según los puntos de corte elegidos
  
  breaks_r <- quantile(df$recency,probs = seq(bins:1)/bins,names = FALSE)
  breaks_f <- quantile(df$frequency,probs = seq(bins:1)/bins,names = FALSE)
  breaks_m <- quantile(df$monetary,probs = seq(bins:1)/bins,names = FALSE)
  
  
  #creo la tabla de puntos de corte
  
  #Armo el umbral minimo
  lower_recency <- as.vector(0)
  lower_frequency <- as.vector(1)
  lower_monetary <- as.vector(0.01)
  
  for(i in 1:bins-1){
    
    lower_recency <- rbind(lower_recency,breaks_r[i]+1)
    lower_frequency <- rbind(lower_frequency,breaks_f[i]+1)
    lower_monetary <- rbind(lower_monetary,breaks_m[i]+1)
  }
  
  #Armo el umbral máximo
  upper_recency <- lower_recency[2]
  upper_frequency <- lower_frequency[2]
  upper_monetary <- lower_monetary[2]
  
  for(i in 2:bins){
    
    upper_recency <- rbind(upper_recency,breaks_r[i]+1)
    upper_frequency <- rbind(upper_frequency,breaks_f[i]+1)
    upper_monetary <- rbind(upper_monetary,breaks_m[i]+1)
  }
  
  threshold <- data.frame(lower_recency=lower_recency,upper_recency=upper_recency,lower_frequency=lower_frequency,upper_frequency,
                          lower_monetary=lower_monetary,upper_monetary= upper_monetary)
  
  
  row.names(threshold) <- NULL
  
  
  #Clasifico a los Usuarios según los puntos de corte
  
  df$recency_cut <- 0
  df$frequency_cut <- 0
  df$monetary_cut <- 0
  
  for(i in seq_len(bins)){
    
    df$recency_cut[((dplyr::between(df$recency,lower_recency[i],upper_recency[i])))] <- bins-(i-1) #reordeno el scoring del recency
    df$frequency_cut[((dplyr::between(df$frequency,lower_frequency[i],upper_frequency[i])))] <- i
    df$monetary_cut[((dplyr::between(df$monetary,lower_monetary[i],upper_monetary[i])))] <- i
  }
  
  #Armo el gráfico de RFM mostrando el promedio de monetary por la combinación
  #de todas las categorías de recency y frecuency
  
  #Creo la paleta de colores
  
  pal <- RColorBrewer::brewer.pal(bins,name = "BuPu")
  
  ##%######################################################%##
#                                                          #
####                  armo el gráfico                   ####
#                                                          #
##%######################################################%##

  
  #Creo el df para el gráfico
  
  df.graf <- dplyr::group_by(df,!!rlang::sym(group_by),recency_cut,frequency_cut)
  df.graf <- dplyr::summarise(df.graf,
                              monetary_mean= mean(monetary))
  
  #Creo el gráfico
  
  grafico_heatmap <- ggplot2::ggplot(df.graf,
                                     ggplot2::aes(x= frequency_cut, y= recency_cut, fill= monetary_mean))+
    ggplot2::geom_tile(color = "white",
                       lwd = .3,
                       linetype = 1,
                       stat = "identity") +
    ggplot2::scale_fill_gradientn(colours = pal) +
    ggplot2:: labs(x= "Frequency", y= "Recency", fill= "Monetary (Prom.)",
                   title= paste0("RFM ",df.graf[1,group_by]))+
    ggplot2::coord_fixed()
  
  return(list("resultado_rfm"=df, "threshold"=threshold, "heatmap"=grafico_heatmap))
  
}