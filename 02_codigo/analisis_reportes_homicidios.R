### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/paquetes_setup_tema.R")

### Extraer datos de tablas en reportes -----

## Generar vector con fechas de reportes ----
dia_mes <- 
  tibble(fecha = seq(as.Date("2019-04-03"), length = 223, by = "1 day"),
         dia = str_pad(day(fecha), width = 2, pad = "0"),
         mes = str_pad(month(fecha), width = 2, pad = "0"),
         dia_mes = str_c(dia, mes, sep = "")) %>% 
  select(dia_mes) %>% 
  pull()

## Generar tibble vacío ----
bd <- tibble(datos = NA,
             entidad = NA,
             numero = NA, 
             fecha = NA)

## Loop para extraer texto de tablas ----

for (i in seq_along(dia_mes)) {
  
  ## Leer texto en la primera hoja ----
  texto <- 
    pdf_text(pdf = str_c("01_datos/reportes/gpo_interinstitucional/reporte_gpo_interinstitucional_", dia_mes[i], "2019.pdf", sep = ""))[[1]]
  
  ## Extraer datos de las dos columnas ----
  tabla_dos_cols <- 
    as_tibble(texto) %>% 
    # Separar renglones usando los linebreaks (\n)
    separate_rows(value, sep = "\\n") %>% 
    # Quitar espacios en blanco innecesarios y notas al pies
    mutate(value = str_trim(value),
           value = str_squish(value),
           value = str_replace(value, "a1", "a"),
           value = str_replace(value, "e1", "e"),
           value = str_replace(value, "í1", "í"),
           value = str_replace(value, "o1", "o"),
           value = str_replace(value, "n1", "n"),
           value = str_replace(value, "s1", "s"),
           value = str_replace(value, "t1", "t"),
           value = str_replace(value, "z1", "z"),
           value = str_replace(value, "\\*", "")) %>% 
    # Filtrar renglones que no correspondan a una entidad
    filter(!str_detect(value, "Víctimas repo"),
           !str_detect(value, "\\(Fisca"),
           !str_detect(value, "Entidades"),
           !str_detect(value, "Fuente"),
           !str_detect(value, "Fosa"),
           !str_detect(value, "El día"),
           !str_detect(value, "Dato obtenido"),
           !str_detect(value, "cuerpos ubicados"),
           !str_detect(value, "Abril"),
           !str_detect(value, "Junio"),
           !str_detect(value, "Septiembre"),
           !str_detect(value, "verificados"),
           !str_detect(value, "corresponden"),
           !str_detect(value, "Bavispe"),
           !str_detect(value, "Los")) %>% 
    # Separar valores correspondientes a dos estados y ponerlo en dos columnas 
    separate(value, into = c("col_1",  "col_2"), 
             sep = "(?<=([0-9] ))",
             remove = T) %>% 
    # Quitar espacios en blanco innecesarios
    mutate(col_1 = str_trim(col_1),
           col_1 = str_squish(col_1),
           col_2 = str_trim(col_2),
           col_2 = str_squish(col_2),
           col_1 = ifelse(is.na(col_1), "dummy", col_1),
           col_2 = ifelse(is.na(col_2), "dummy", col_2)) %>%
    # Filtrar renglones en donde col_1 y col_2 tienen el mismo contenido
    filter(!(col_1 == col_2)) %>%
    # Imprimir (nomás pa ver qué está pasando :))
    print(n = Inf)
  
  ## Generar tibble con variables separadas -----
  
  # Una columna registra el nombre de la entidad entidad y otra para el número de homicidios dolosos del día correspondiente
  tabla_datos_separados <- 
    tibble(datos = c(tabla_dos_cols$col_1, tabla_dos_cols$col_2)) %>%
    # Eliminar renglones donde datos tiene valor "dummy", "" o incluye el patrón "Total"
    filter(!str_detect(datos, "Total"),
           datos != "dummy",
           datos != "") %>% 
    # Extrar datos de entidad y número de homicidios
    mutate(entidad = str_replace(datos, " [0-9]*$", ""),
           numero = str_extract_all(datos, "\\(?[0-9,.]+\\)?")) %>% 
    # Desanidar número
    unnest(numero) %>%
    # Transformar clase de variable numero y generar variable fecha
    mutate(numero = as.numeric(numero),
           fecha = make_date(2019, str_sub(string = dia_mes[i], start = 3, end = 4), str_sub(string = dia_mes[i], start = 1, end = 2))) %>% 
    print(n = Inf)
  
  ## Guardar resultado en el tibble creado antes de arrancar el loop
  bd <- 
    bind_rows(bd, tabla_datos_separados) %>% 
    filter(!is.na(datos)) # Eliminar NAs introducidos al crear el tibble
}

### Identificar observaciones con problemas potenciales ----

bd %>% 
  count(entidad, sort = T) %>% 
  print(n = Inf)

bd %>% 
  filter(entidad %in% c("0", "1", "2", "3", "7", "8", "10"))


### Reemplazar manualmente observaciones con problemas ----

# Para entender cuál era el problema específico en cada caso, (i) revisé el archivo PDF original y (ii) corrí el script "verificacion_datos.R" ajustando la fecha de acuerdo con el tibble obtenido en el paso anterior. A partir de esto determiné cuál era el problema en cada circunstancia y lo corregí manualmente en el código a continuación

bd <- 
  bd %>% 
  mutate(entidad = case_when(fecha == as_date("2019-05-12") & entidad == "3" ~ "Ciudad de México",
                             fecha == as_date("2019-05-12") & entidad == "2" ~ "Coahuila",
                             fecha == as_date("2019-05-12") & entidad == "1" ~ "Zacatecas",
                             fecha == as_date("2019-05-26") & entidad == "Guanajuato Tamaulipas" ~ "Tamaulipas",
                             fecha == as_date("2019-05-26") & entidad == "8" ~ "Guanajuato",
                             fecha == as_date("2019-06-27") & entidad == "Durango Sonora" ~ "Sonora",
                             fecha == as_date("2019-06-27") & entidad == "0" ~ "Durango",
                             fecha == as_date("2019-06-28") & entidad == "Durango Sonora" ~ "Sonora",
                             fecha == as_date("2019-06-28") & entidad == "0" ~ "Durango",
                             fecha == as_date("2019-06-29") & entidad == "Estado de México Tabasco" ~ "Tabasco",
                             fecha == as_date("2019-06-29") & entidad == "7" ~ "Estado de México",
                             fecha == as_date("2019-06-30") & entidad == "Estado de México Tabasco" ~ "Tabasco",
                             fecha == as_date("2019-06-30") & entidad == "10" ~ "Estado de México",
                             fecha == as_date("2019-07-01") & entidad == "Durango Sonora" ~ "Sonora",
                             fecha == as_date("2019-07-01") & entidad == "0" ~ "Durango",
                             fecha == as_date("2019-07-03") & entidad == "Durango Sonora" ~ "Sonora",
                             fecha == as_date("2019-07-03") & entidad == "0" ~ "Durango",
                             fecha == as_date("2019-07-26") & entidad == "Durango Sonora" ~ "Sonora",
                             fecha == as_date("2019-07-26") & entidad == "0" ~ "Durango",
                             fecha == as_date("2019-09-08") & entidad == "1" ~ "Colima",
                             TRUE ~ entidad)) 


### Crear variable "modificada" ----

# Esta variable permite identificar las observaciones que fueron modificadas manualmente en el paso anterior

bd <- 
  bd %>% 
  mutate(modificada = case_when(fecha == as_date("2019-05-12") & entidad == "Ciudad de México" ~ 1,
                                fecha == as_date("2019-05-12") & entidad == "Coahuila" ~ 1,
                                fecha == as_date("2019-05-12") & entidad == "Zacatecas" ~ 1,
                                fecha == as_date("2019-05-26") & entidad == "Tamaulipas" ~ 1,
                                fecha == as_date("2019-05-26") & entidad == "Guanajuato" ~ 1,
                                fecha == as_date("2019-06-27") & entidad == "Sonora" ~ 1,
                                fecha == as_date("2019-06-27") & entidad == "Durango" ~ 1,
                                fecha == as_date("2019-06-28") & entidad == "Sonora" ~ 1,
                                fecha == as_date("2019-06-28") & entidad == "Durango" ~ 1,
                                fecha == as_date("2019-06-29") & entidad == "Tabasco" ~ 1,
                                fecha == as_date("2019-06-29") & entidad == "Estado de México" ~ 1,
                                fecha == as_date("2019-06-30") & entidad == "Tabasco" ~ 1,
                                fecha == as_date("2019-06-30") & entidad == "Estado de México" ~ 1,
                                fecha == as_date("2019-07-01") & entidad == "Sonora" ~ 1,
                                fecha == as_date("2019-07-01") & entidad == "Durango" ~ 1,
                                fecha == as_date("2019-07-03") & entidad == "Sonora" ~ 1,
                                fecha == as_date("2019-07-03") & entidad == "Durango" ~ 1,
                                fecha == as_date("2019-07-26") & entidad == "Sonora" ~ 1,
                                fecha == as_date("2019-07-26") & entidad == "Durango" ~ 1,
                                fecha == as_date("2019-09-08") & entidad == "Colima" ~ 1,
                                TRUE ~ 0))

### Generar tibble con cifras por mes y estado ----
reportes_x_mes <- 
  bd %>% 
  filter(fecha < as_date("2019-10-01")) %>% 
  mutate(fecha_piso = floor_date(fecha, unit = "month")) %>% 
  group_by(entidad, fecha_piso) %>% 
  summarise(num = sum(numero)) %>% 
  ungroup()


### Importar datos de víctimas del SNSP ----

# Aquí vas a tener que ajustar el código para que después de descargar la base de datos de víctimas de esta liga https://drive.google.com/file/d/1iifsxmh1LFz8DUFaPR17H3_IveR6WOAZ/view, la puedas importar
victimas <- 
  read_excel("../../../../10 recursos/datos/snsp/victimas/Estatal-V°ctimas - septiembre 2019.xlsx") %>% 
  clean_names()

### Tidyear datos de víctimas ----
victimas <- 
  victimas %>% 
  gather(enero:diciembre, 
         key = "mes",
         value = "numero")

### Generar diversas variables en la  bd victimas ----
victimas <- 
  victimas %>% 
  # Generar variables
  mutate(mes_num = case_when(mes == "enero" ~ 1,
                             mes == "febrero" ~ 2,
                             mes == "marzo" ~ 3,
                             mes == "abril" ~ 4,
                             mes == "mayo" ~ 5,
                             mes == "junio" ~ 6,
                             mes == "julio" ~ 7,
                             mes == "agosto" ~ 8,
                             mes == "septiembre" ~ 9,
                             mes == "octubre" ~ 10,
                             mes == "noviembre" ~ 11,
                             mes == "diciembre" ~ 12), 
         fecha = make_date(ano, mes_num),
         admin = ifelse(fecha < as.Date("2018-12-01"), "Peña Nieto", "AMLO")) %>% 
  # Reordenar variables
  select(fecha, año = ano, mes, mes_num, everything())

### Calcular número de víctimas por mes y estado ----
victimas_x_mes_edo <- 
  victimas %>% 
  # Filtrar datos
  filter(fecha > as_date("2019-03-01"),
         subtipo_de_delito == "Homicidio doloso") %>% 
  # Calcular número mensual de vícitimas
  group_by(fecha, entidad) %>% 
  summarise(victimas_x_mes = sum(numero), 
            admin = last(admin)) %>% 
  ungroup() %>% 
  mutate(entidad = case_when(str_detect(entidad, "Coahuila") ~ "Coahuila",
                             str_detect(entidad, "Micho") ~ "Michoacán",
                             entidad == "México" ~ "Estado de México",
                             str_detect(entidad, "Veracruz") ~ "Veracruz",
                             TRUE ~ entidad))

### Unir datos de reportes diarios y víctimas ----
bd_todo <- 
  reportes_x_mes %>% 
  left_join(victimas_x_mes_edo, by = c("fecha_piso" = "fecha", "entidad")) %>% 
  pivot_longer(num:victimas_x_mes, 
               names_to = "serie", 
               values_to = "numero") %>% 
  mutate(serie = ifelse(serie == "num", "Reporte diario", "Víctimas carpetas de investigación"))


### Gráfica víctimas mensuales de homicidio doloso de acuerdo con el reporte diario de la CNS y el reporte mensual del SNSP ----

bd_todo %>% 
  ggplot() +
  geom_line(aes(fecha_piso, numero, 
                group = serie, 
                color = serie),
            size = 1) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(values = c("#EE4F42", "#06A2BC")) +
  facet_wrap(~ str_wrap(entidad, width = 15), ncol = 8, scales = "free_y") +
  labs(title = str_wrap(str_to_upper("víctimas mensuales de homicidio doloso de acuerdo con el reporte diario de la CNS y el reporte mensual del SNSP",), width = 70),
       x = "",
       y ="Número",
       color = NULL,
       caption = "@segasi / @JMarquezP / Fuente: SNSP y CNS") +
  tema + 
  theme(panel.grid = element_line(linetype = 3, size = 0.6, color = "grey90"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "grey40", color = "grey40"),
        strip.text = element_text(color = "white", size = 12),
        legend.position = c(0.82, -0.1),
        legend.direction = "horizontal") +
  ggsave("03_graficas/comparacion_mensual_reporte_vs_victimas_homicidio_doloso.png", width = 16, height = 10, dpi = 200)