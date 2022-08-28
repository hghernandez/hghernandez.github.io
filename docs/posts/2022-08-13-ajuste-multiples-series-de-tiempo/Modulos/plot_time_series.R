plot_time_series <- function(df,title,y_lab,x,y,facet){
  
  
  library(ggplot2)
  
  theme_geo <- function(){
    
    ggplot2:: theme_grey() %+replace%
      ggplot2::theme(
        strip.text.x = element_text(
          size = 10, color = "#191C3C", face = "bold.italic"),
        axis.title.x= element_blank(),
        axis.title.y= element_text(face= "bold", size= 9, colour = "#191C3C",angle = 90),
        axis.text.x = element_text(face= "bold", size= 9, colour = "#191C3C", angle = 30,vjust = 0.9, hjust = 1),
        plot.title = element_text(hjust = 0.5,face= "bold", size= 11, colour = "#191C3C")
      )
  }
  
x = rlang::sym(x)
y = rlang::sym(y)
facet = rlang::sym(facet)

grafico <- ggplot(df, aes(x= !!sym(x),y= !!sym(y)))+
  geom_line(data = df %>% filter(model_details== 'ACTUAL'), aes(x= !!sym(x),y= !!sym(y)),
            color= "#FF8EDD")+
  geom_line(data= df %>% filter(model_details != 'ACTUAL'),
            aes(x= !!sym(x),y= !!sym(y)), color= "#9D00FF",
            linetype= "dashed", lineend="round")+ 
  geom_ribbon(data=df %>% filter(model_details != 'ACTUAL'),
              aes(ymin = conf_lo, ymax = conf_hi), alpha = 0.09)+
  scale_x_date(date_labels = "%b %Y",date_breaks = "12 month")+
  labs(title= title, y= y_lab)+
  scale_y_continuous(labels = scales::label_number_si())+
  facet_wrap(vars(!!sym(facet)), ncol = 3,scales = "free_y") +
  theme_geo()

  
}