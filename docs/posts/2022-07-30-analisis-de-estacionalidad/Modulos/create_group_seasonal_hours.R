#Creo la funciÃ³n para el diagnÃ³stico de estacionalidad


create.group_seasonal.hours <- function(df, date, status){
  
  hora <- df %>%
    filter(status_normalizado== status) %>%
    mutate(hora= hour(!!sym(date))) %>%
    group_by(nombre,hora)%>%
    summarise(Transactions= sum(Transactions))%>%
    mutate(grupo= "Hora",
           hora= factor(hora)) %>%
    rename("agrupamiento"= hora) %>%
    as.data.frame()
  
  
  dia <- df %>%
    filter(status_normalizado== status) %>%
    mutate(dia= wday(!!sym(date),label = T,abbr = F)) %>%
    group_by(nombre,dia)%>%
    summarise(Transactions= sum(Transactions))%>%
    mutate(grupo= "Dia",
           dia= factor(dia)) %>%
    rename("agrupamiento"= dia) %>%
    as.data.frame()
  
  df.group <- rbind(hora,dia) %>% as.data.frame()
  
  # Ordeno para que aparezcan dia, semana, mes
  
  df.group$grupo <- factor(df.group$grupo, levels= c("Hora","Dia"))
  
  
  
  
  #Creo el theme de geopagos
  
  theme_geo <- function(){
    
    ggplot2:: theme_grey() %+replace%
      ggplot2::theme(
        strip.text.x = element_text(
          size = 10, color = "#191C3C", face = "bold.italic"),
        axis.title.x= element_blank(),
        axis.title.y= element_text(face= "bold", size= 9, colour = "#191C3C",angle = 90),
        axis.text.x = element_text(face= "bold", size= 9, colour = "#191C3C"),
        plot.title = element_text(hjust = 0.5,face= "bold", size= 11, colour = "#191C3C")
      )
  }
  
  ggplot(df.group, aes(agrupamiento,Transactions, fill= Transactions))+
    geom_bar(stat = "identity")+
    scale_y_continuous(labels = scales::label_number_si())+
    scale_fill_gradient(low = "#FF8EDD",high = "#9D00FF", guide = "none")+
    labs(y= "Transacciones", title = stringr::str_to_title(hora[1,1]))+
    facet_wrap(~grupo, nrow = 3, scales = "free")+
    theme_geo()
  
}