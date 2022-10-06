library(blastula)

email <- render_email("C:/Users/usuario/Documents/hghernandez.github.io - dev/_posts/2022-10-02-envo-de-email-compartimos-nuestro-reporte/Report_RFM.Rmd")

create_smtp_creds_file(
  file = "gmail_creds",
  user = "hernan.hernandez@geopagos.com",
  host = "smtp.gmail.com",
  port = 465,
  use_ssl = TRUE
)


email %>%
  smtp_send(to = "hernanghernandez@gmail.com",
            from = "hernan.hernandez@geopagos.com",
            subject =  paste0("Reporte RFM ",format(lubridate::ymd(Sys.Date()),"%B %Y")),
            credentials = creds_file(file = "gmail_creds"))






