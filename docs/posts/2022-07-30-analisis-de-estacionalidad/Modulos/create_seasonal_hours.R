#Creo la funciÃ³n para el diagnÃ³stico de estacionalidad


create.seasonal.hours <- function(df, date){
  
  hora <- df %>%
      #filter(status_normalizado== status) %>%
      mutate(hora= hour(!!sym(date))) %>%
      #group_by(hora)%>%
      #summarise(Transactions= sum(Transactions))%>%
      mutate(grupo= "Hora",
             hora= factor(hora)) %>%
      rename("agrupamiento"= hora)
  
 
  
  
  # dia <- df %>%
  #   filter(status_normalizado== status) %>%
  #   mutate(dia= wday(!!sym(date),label = T,abbr = F)) %>%
  #   #group_by(dia)%>%
  #   #summarise(Transactions= sum(Transactions))%>%
  #   mutate(grupo= "Dia",
  #          dia= factor(dia)) %>%
  #   rename("agrupamiento"= dia)
  # 
  # 
  # semana <- df %>%
  #   filter(status_normalizado== status) %>%
  #   mutate(semana= week(!!sym(date))) %>%
  #   #group_by(mes)%>%
  #   #summarise(Transactions= sum(Transactions))%>%
  #   mutate(grupo= "Semana",
  #          semana= factor(semana)) %>%
  #   rename("agrupamiento"= semana)
  # 
  # 
  # mes <- df %>%
  #   filter(status_normalizado== status) %>%
  #   mutate(mes= month(!!sym(date),label = T,abbr = F)) %>%
  #   #group_by(mes)%>%
  #   #summarise(Transactions= sum(Transactions))%>%
  #   mutate(grupo= "Mes",
  #          mes= factor(mes)) %>%
  #   rename("agrupamiento"= mes)
  # 
  # # quarter <- df %>%
  # #   filter(status_normalizado== status) %>%
  # #   mutate(quarter= quarter(Date)) %>%
  # #   #group_by(quarter)%>%
  # #   #summarise(Transactions= sum(Transactions)) %>%
  # #   mutate(grupo= "Trimestre",
  # #          quarter= factor(quarter)) %>%
  # #   rename("agrupamiento"= quarter)





  # df.group <- rbind(hora,dia,semana,mes) %>% as.data.frame()
  # 
  # 
  # # Ordeno para que aparezcan dia, semana, mes
  # df.group$grupo <- factor(df.group$grupo, levels= c("Hora","Dia","Semana","Mes"))



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

  ggplot(hora, aes(agrupamiento,Transactions))+
    stat_boxplot(geom = "errorbar",
                 width = 0.15,
                 coef = 3) +
    geom_boxplot(outlier.color = "#F452E8", colour = "#191C3C")+
    scale_y_continuous(labels = scales::label_number_si())+
    labs(y= "Transacciones", title = stringr::str_to_title(hora[1,4]))+
    facet_wrap(~grupo, nrow = 3, scales = "free")+
    theme_geo()
  
}