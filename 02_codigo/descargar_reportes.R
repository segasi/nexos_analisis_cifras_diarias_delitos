### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/paquetes_setup_tema.R")


### Generar vector con fechas de reportes ----
dia_mes <- 
  tibble(fecha = seq(as.Date("2019-04-03"), length = 223, by = "1 day"),
         dia = str_pad(day(fecha), width = 2, pad = "0"),
         mes = str_pad(month(fecha), width = 2, pad = "0"),
         dia_mes = str_c(dia, mes, sep = "")) %>% 
  select(dia_mes) %>% 
  pull()

