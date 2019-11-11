### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/paquetes_setup_tema.R")

### Descargar reportes -----

# Generar vector con fechas de reportes
dia_mes <- 
  tibble(fecha = seq(as.Date("2019-04-03"), length = 219, by = "1 day"),
         dia = str_pad(day(fecha), width = 2, pad = "0"),
         mes = str_pad(month(fecha), width = 2, pad = "0"),
         dia_mes = str_c(dia, mes, sep = "")) %>% 
  select(dia_mes) %>% 
  pull()

# Descargar reportes
for (i in seq_along(dia_mes)) {
  download(url = str_c("http://www.informeseguridad.cns.gob.mx/files/homicidios_", dia_mes[i], "2019_v2.pdf", sep = ""), 
           destfile = str_c("01_datos/reportes/gpo_interinstitucional/reporte_gpo_interinstitucional_", dia_mes[i], "2019.pdf", sep = ""))
}


### Extraer datos de tablas en reportes -----

## Generar tibble vacío ----
bd <- tibble(datos = NA,
             entidad = NA,
             numero = NA, 
             fecha = NA)