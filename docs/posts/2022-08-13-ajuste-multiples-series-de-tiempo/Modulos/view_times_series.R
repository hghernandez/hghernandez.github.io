
view_times_series <- function(df,x,y,facet,title,y_lab,scales){
  
  
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
  
  
  
  ggplot2::ggplot(df,
                  ggplot2::aes(x = !!sym(x), y = !!sym(y))) +
    ggplot2::geom_line(data = df,
                       ggplot2::aes(x = !!sym(x), y = !!sym(y)),
                       color = "#FF8EDD") +
    ggplot2::geom_smooth(
      data = df,
      ggplot2::aes(x = !!sym(x), y = !!sym(y)),
      size = 0.5,
      se = FALSE,
      color = "#191C3C"
    ) +
    ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "12 month") +
    ggplot2::labs(title = title, y = y_lab) +
    ggplot2::scale_y_continuous(labels = scales::label_number_si()) +
    ggplot2::facet_wrap(vars(!!sym(facet)), ncol = 4, scales = scales)
  
  
  
  
}