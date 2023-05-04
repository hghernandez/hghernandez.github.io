#Creo la funcion para mapas de calor en un calendario


heatmap.calendar <- function(df,fini,ffin,ColorBrewer){
  
  
  
  #Armo el df del partner
  
  df <- df %>%
    rename("fecha"= Fecha,
           "Transactions"= Cant_Ventas) %>%
    mutate(fecha= ymd(fecha))
  
  #Armo una combinación de todas las fechas entre un periodo
  
  fechas  <- tibble(fecha = seq(
    dmy(fini),
    dmy(ffin),
    "days"
  ))
  
  
  #completamos las fechas que no existen
  
  fechas <- fechas %>%
    left_join(df, by= "fecha") %>%
    #fill(project_name, .direction = "downup")%>%
    #fill(status_normalizado, .direction = "downup")%>%
    mutate(Transactions= replace_na(Transactions,0))
  
  
  
  data.compl <- fechas %>% mutate(
    weekday = wday(fecha, label = T, week_start = 7,abbr = T), 
    month = month(fecha, label = T),
    date = yday(fecha),
    week = epiweek(fecha)
  )
  
  
  data.compl$week[data.compl$month=="ene" & data.compl$week ==52] = 0
  
  data.compl = data.compl %>% 
    group_by(month) %>% 
    mutate(monthweek = 1 + week - min(week)) 
  
  
  #Fijo la escala de gradientes
  
  pal <- RColorBrewer::brewer.pal(5,ColorBrewer)
  
  library(ggplot2)
  
  graf <- data.compl %>%
    ggplot(aes(weekday,-week, fill= Transactions)) +
    geom_tile(colour = "white") +
    labs(title= stringr::str_to_title(data.compl[1,3]))  + 
    geom_text(aes(label = day(fecha)), size = 2.5, color = "white") +
    scale_fill_gradientn(colours= pal, na.value = 'white')+
    facet_wrap(~month, nrow = 3, ncol = 4, scales = "free")+
    theme(aspect.ratio = 1/2,
          legend.position = "none",
          legend.key.width = unit(3, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=10),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.title.align = 0.5,
          strip.background = element_blank(),
          strip.text = element_text(face = "bold", size = 14,colour = "#191C3C"),
          panel.border = element_rect(colour = "grey", fill=NA, size=1),
          plot.title = element_text(hjust = 0, size = 14, face = "bold",
                                    margin = margin(0,0,0.5,0, unit = "cm"))
    )
       
     
          
 
  
}