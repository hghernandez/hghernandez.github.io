segment_rfm <- function(tabla_rfm,
                        nombres_segmentos=NULL,
                        recency_lower=NULL,
                        recency_upper=NULL,
                        frequency_lower=NULL,
                        frequency_upper=NULL,
                        monetary_lower=NULL,
                        monetary_upper=NULL) {
  #Crea los segmentos
  segment <- NULL
  
  data <- tabla_rfm$resultado_rfm
  data$segmento <- 1
  
  n_segments <- length(nombres_segmentos)
  
  for (i in seq_len(n_segments)) {
    
    data$segmento[(
      (dplyr::between(data$recency_cut,recency_lower[i], recency_upper[i])) &
        (dplyr::between(data$frequency_cut,frequency_lower[i],frequency_upper[i])) &
        (dplyr::between(data$monetary_cut,monetary_lower[i], monetary_upper[i])) &
        !data$segmento %in% nombres_segmentos)] <- nombres_segmentos[i]
  }
  
  
  data$segmento[is.na(data$segmento)] <- "Usuals"
  data$segmento[data$segmento == 1]<- "Usuals" 
  
  ##%######################################################%##
  #                                                          #
  ####    Arma tabla de distribución de los segmentos     ####
  #                                                          #
  ##%######################################################%##
  
  distribucion <- dplyr::group_by(data,segmento)
  distribucion <- dplyr::summarise(distribucion,cantidad= n_distinct(user_id))
  distribucion <- dplyr::mutate(distribucion,'%'= round(cantidad*100/sum(cantidad),1))
  distribucion <- dplyr::arrange(distribucion,desc(cantidad))
  
##%######################################################%##
#                                                          #
####        Arma el impacto de cada segmento en         ####
####         la cantidad de ventas y los montos         ####
#                                                          #
##%######################################################%##

  impacto <- dplyr::group_by(data,segmento)
  impacto <- summarise(impacto, Ventas= sum(frequency),
            Monto = sum(monetary))
  impacto <- mutate(impacto,'%_Ventas'= round(Ventas*100/sum(Ventas),1),
           '%_Monto'= round(Monto*100/sum(Monto),1))
  impacto <- select(impacto,segmento,Ventas,`%_Ventas`,Monto,`%_Monto`)
  impacto <-arrange(impacto,desc(Ventas))
  
  
  ##%######################################################%##
  #                                                          #
  ####    Arma distribución de los scores de monetary     ####
  ####      entre los scores de recency y frequency       ####
  #                                                          #
  ##%######################################################%##
  
  
  #Creo el theme geo
  
  theme_geo <- function(){
    
    ggplot2::'%+replace%'
    ggplot2::theme_grey()
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 11, color = "#191C3C", face = "bold"),
      axis.title.x=  ggplot2::element_text(hjust = 0.5,face= "bold", size= 11, colour = "#191C3C"),
      axis.title.y=  ggplot2::element_text(face= "bold", size= 9, colour = "#191C3C",angle = 90),
      axis.text.y= ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.x =  ggplot2::element_text(face= "bold", size= 9, colour = "#191C3C",vjust = 0.9, hjust = 1),
      plot.title =  ggplot2::element_text(hjust = 0.5,face= "bold", size= 11, colour = "#191C3C")
    )
  }
  
  #Arma el grafico
  
  bar_chart <- ggplot2::ggplot(data,ggplot2::aes(x=monetary_cut))+
    ggplot2::geom_bar(fill= "#FF8EDD")+
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ ., name = "Recency"))+
    ggplot2::labs(x= "Monetary",title = "Frequency", y= " ")+
    ggplot2::facet_grid(factor(recency_cut,stringr::str_sort(unique(recency_cut),decreasing = TRUE, numeric = TRUE)) ~ frequency_cut) +
    theme_geo()
  
  #Arma gráfico de barras de los segmentos
  
  
  bar_chart_segmentos <- ggplot2::ggplot(distribucion, ggplot2::aes(x= reorder(segmento,cantidad), y= cantidad, fill= cantidad))+
    ggplot2::geom_bar(stat= 'identity')+
    ggplot2::scale_y_continuous(labels = scales::label_number_si())+
    ggplot2::scale_fill_gradient(low = "#FF8EDD",high = "#9D00FF", guide = "none")+
    ggplot2::geom_text(data = distribucion,ggplot2::aes(label= paste0(`%`,"%"),fontface= "bold"),
                       vjust= 1, hjust = -0.1, size= 2.25)+
    ggplot2::labs(y= "Cantidad", x= " ", title= data[1,1])+
    ggplot2::coord_flip()+
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(
        size = 10, color = "#191C3C", face = "bold.italic"),
      axis.title.x= ggplot2::element_blank(),
      axis.text.y= ggplot2::element_text(face= "bold", size= 9, colour = "#191C3C"),
      axis.text.x = ggplot2::element_text(face= "bold", size= 9, colour = "#191C3C"),
      plot.title = ggplot2::element_text(hjust = 0.5,face= "bold", size= 11, colour = "#191C3C")
    )
  
  return(list("tabla_rfm"=data,"bar_chart"=bar_chart,"Composicion_segmento"=distribucion,
              "bar_chart_seg"= bar_chart_segmentos,"impact_segment"=impacto))
}