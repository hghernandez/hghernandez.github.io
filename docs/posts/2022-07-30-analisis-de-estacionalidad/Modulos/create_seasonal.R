#Creo la función para el diagnóstico de estacionalidad


create.seasonal <- function(df, date){
  
  # hora <- df %>%
  #     mutate(hora= hour(Created_Local)) %>%
  #     #group_by(dia)%>%
  #     #summarise(Transactions= sum(Transactions))%>%
  #     mutate(grupo= "Hora",
  #            hora= factor(hora)) %>%
  #     rename("agrupamiento"= hora)
  
  
  
  
  # hora <- df %>%
  #     mutate(hora= hour(Created_Local)) %>%
  #     #group_by(dia)%>%
  #     #summarise(Transactions= sum(Transactions))%>%
  #     mutate(grupo= "Hora",
  #            hora= factor(hora)) %>%
  #     rename("agrupamiento"= hora)
  
  
  dia <- df %>%
    #filter(status_normalizado== status) %>%
    mutate(dia= wday(!!sym(date),label = T,abbr = T)) %>%
    #group_by(dia)%>%
    #summarise(Transactions= sum(Transactions))%>%
    mutate(grupo= "Dia",
           dia = as.factor(dia))%>%
    rename("agrupamiento"= dia) %>%
    as.data.frame()
  
  semana <- df %>%
    #filter(status_normalizado== status) %>%
    mutate(semana= epiweek(!!sym(date))) %>%
    #group_by(mes)%>%
    #summarise(Transactions= sum(Transactions))%>%
    mutate(grupo= "Semana",
           semana = as.factor(semana)) %>%
    rename("agrupamiento"= semana)%>%
    as.data.frame()
  
  
  mes <- df %>%
    #filter(status_normalizado== status) %>%
    mutate(mes= month(!!sym(date),label = T,abbr = T)) %>%
    #group_by(mes)%>%
    #summarise(Transactions= sum(Transactions))%>%
    mutate(grupo= "Mes") %>%
    rename("agrupamiento"= mes) %>%
    as.data.frame()
  
  quarter <- df %>%
    #filter(status_normalizado== status) %>%
    mutate(quarter= quarter(!!sym(date))) %>%
    #group_by(quarter)%>%
    #summarise(Transactions= sum(Transactions)) %>%
    mutate(grupo= "Trimestre",
           quarter= factor(quarter)) %>%
    rename("agrupamiento"= quarter)%>%
    as.data.frame()
  
  
  df.group <- rbind(dia,semana,mes,quarter) %>% as.data.frame()
  
  
  # Ordeno para que aparezcan dia, semana, mes
  
  df.group$grupo <- factor(df.group$grupo, levels= c("Dia","Semana","Mes","Trimestre"))
  
  
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
  
  graf <- ggplot(df.group, aes(agrupamiento,Cant_Ventas))+
    stat_boxplot(geom = "errorbar",
                 width = 0.15,
                 coef = 1.5) +
    geom_boxplot(outlier.color = "#F452E8", colour = "#191C3C")+
    scale_y_continuous(labels = scales::label_number_si())+
    labs(y= "Cant_Ventas", title = paste0("Estacionalidad ",df.group[1,3]))+
    facet_wrap(~grupo, nrow = 3, scales = "free")+
    theme_geo()
  
  
  return(list("grafico"=graf, "data"= df.group))
  
}