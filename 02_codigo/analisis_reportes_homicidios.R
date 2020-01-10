### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/paquetes_setup_tema.R")

### Extraer datos de tablas en reportes -----

## Generar vector con fechas de reportes ----
dia_mes <- 
  tibble(fecha = seq(as.Date("2019-04-03"), length = 242, by = "1 day"),
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
  filter(entidad %in% c("0", "1", "2", "3", "7", "8", "10", "Durango Sonora", "Estado de México Tabasco", "Guanajuato Tamaulipas", "3", "7", "8", "10"))


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

### Volver a calcular frecuencia de nombre de entidades

# Si las correcciones manuales hechas en el paso anterior son correctas, entonces cada entidad debería tener la misma frecuencia

bd %>% 
  count(entidad, sort = T) %>% 
  print(n = Inf)

### Verificar todas las observaciones para aquellas fechas que presentaron uno o más errores

bd %>% filter(fecha == as_date("2019-05-12")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-05-26")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-06-27")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-06-28")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-06-29")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-06-30")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-07-01")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-07-03")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-07-26")) %>% arrange(entidad) %>% print(n = Inf)

bd %>% filter(fecha == as_date("2019-09-08")) %>% arrange(entidad) %>% print(n = Inf)



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
  filter(fecha < as_date("2019-12-01")) %>% 
  mutate(fecha_piso = floor_date(fecha, unit = "month")) %>% 
  group_by(entidad, fecha_piso) %>% 
  summarise(num = sum(numero)) %>% 
  ungroup()


### Importar datos de víctimas del SNSP ----

# Aquí vas a tener que ajustar el código para que después de descargar la base de datos de víctimas de esta liga https://drive.google.com/file/d/1HD4vP9_uPwKJs-Yu9Yg1ywiP3eWupA_q/view, la puedas importar

# Ojo: aquí vas a tener que ajustar la ruta para poder acceder al archivo después de que lo hayas descargado
victimas <- 
  read_excel("../../../../10 recursos/datos/snsp/victimas/Estatal-Víctimas - noviembre 2019.xlsx") %>% 
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
  group_by(fecha_piso, serie) %>% 
  summarise(numero = sum(numero)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(fecha_piso, numero, color = serie),
            size = 1.5) +
  scale_y_continuous(labels = comma, breaks = seq(2000, 3000, 100)) + 
  scale_color_manual(values = c("#EE4F42", "#06A2BC")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "VÍCTIMAS MENSUALES DE HOMICIDIO DOLOSO DE ACUERDO CON EL<br><span style='color:#EE4F42'>REPORTE DIARIO</span> Y EL <span style='color:#06A2BC'>REPORTE MENSUAL DEL SNSP</span> (2019)",
       x = "\n",
       y ="Número\n",
       color = NULL,
       caption = "@segasi / Fuente: SNSP y CNS") +
  tema + 
  theme(panel.grid = element_line(linetype = 3, size = 0.6, color = "grey90"),
        plot.title = element_markdown(lineheight = 1.3, size = 28, face = "bold", margin = margin(10,0,20,0), family = "Lato Bold"),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 20),
        legend.position = c(0.75, -0.12),
        legend.direction = "horizontal") +
  ggsave("03_graficas/comparacion_mensual_reporte_vs_victimas_homicidio_doloso_nacional.png", width = 16, height = 10, dpi = 200)

### Calcular diferencias porcentuales entre las líneas roja y azul de la primera gráfica del artículo ----

bd_todo %>% 
  group_by(fecha_piso, serie) %>% 
  summarise(numero = sum(numero)) %>% 
  mutate(diferencia = numero - lag(numero),
         diferencia_porcentual = diferencia/numero*100) %>% 
  ungroup()  


### Gráfica víctimas mensuales de homicidio doloso de acuerdo con el reporte diario de la CNS y el reporte mensual del SNSP, por etidad ----

bd_todo %>% 
  ggplot() +
  geom_line(aes(fecha_piso, numero, 
                group = serie, 
                color = serie),
            size = 1) +
  scale_y_continuous(labels = comma) + 
  scale_color_manual(values = c("#EE4F42", "#06A2BC")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  facet_wrap(~ str_wrap(entidad, width = 15), ncol = 8, scales = "free_y") +
  labs(title = "VÍCTIMAS MENSUALES DE HOMICIDIO DOLOSO DE ACUERDO CON EL<span style='color:#EE4F42'> REPORTE<br>DIARIO</span> Y EL <span style='color:#06A2BC'>REPORTE MENSUAL DEL SNSP</span>, POR ENTIDAD (2019)",
       x = "",
       y ="Número",
       color = NULL,
       caption = "@segasi / Fuente: SNSP y CNS") +
  tema + 
  theme(panel.grid = element_line(linetype = 3, size = 0.6, color = "grey90"),
        plot.title = element_markdown(lineheight = 1.3, size = 28, face = "bold", margin = margin(10,0,20,0), family = "Lato Bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        strip.background = element_rect(fill = "grey40", color = "grey40"),
        strip.text = element_text(color = "white", size = 12),
        legend.position = c(0.82, -0.1),
        legend.direction = "horizontal") +
  ggsave("03_graficas/comparacion_mensual_reporte_vs_victimas_homicidio_doloso_por_edo.png", width = 16.5, height = 10, dpi = 200)


### Calcular número y % de coincidencia en el sentido del cambio mensual de la serie usando datos del reporte diario y de la bd de víctimas del SNSP, todos los estados ----
reportes_x_mes %>% 
  # Unir datos de reportes diarios y víctimas
  left_join(victimas_x_mes_edo, by = c("fecha_piso" = "fecha", "entidad")) %>% 
  group_by(entidad) %>% 
  # Calcular cambio mensual de homicidios de acuerdo con datos del reporte diario
  mutate(cambio_num = num - lag(num), 
         # Calcular cambio mensual de víctimas de acuerdo con datos del SNSP
         cambio_victimas = victimas_x_mes - lag(victimas_x_mes),
         # Crear variable para identificar si el sentido del cambio en ambas series coincide
         coinciden = ifelse(cambio_num > 0 & cambio_victimas > 0 | cambio_num < 0 & cambio_victimas < 0, "Coinciden", "No coinciden")) %>% 
  ungroup() %>%
  # Eliminar las observaciones correspondientes al primer mes para todos los estados
  filter(!is.na(coinciden)) %>% 
  # Calcular frecuencias
  count(coinciden) %>% 
  # Calcular porcentajes
  mutate(por = n/sum(n)) 


### Calcular número y % de coincidencia en el sentido del cambio mensual de la serie usando datos del reporte diario y de la bd de víctimas del SNSP, por estado ----
reportes_x_mes %>% 
  # Unir datos de reportes diarios y víctimas
  left_join(victimas_x_mes_edo, by = c("fecha_piso" = "fecha", "entidad")) %>%
  group_by(entidad) %>% 
  # Calcular cambio mensual de homicidios de acuerdo con datos del reporte diario
  mutate(cambio_num = num - lag(num), 
         # Calcular cambio mensual de víctimas de acuerdo con datos del SNSP
         cambio_victimas = victimas_x_mes - lag(victimas_x_mes),
         # Crear variable para identificar si el sentido del cambio en ambas series coincide
         coinciden = ifelse(cambio_num > 0 & cambio_victimas > 0 | cambio_num < 0 & cambio_victimas < 0, "Coinciden", "No coinciden")) %>% 
  ungroup() %>% 
  # Eliminar las observaciones correspondientes al primer mes para todos los estados
  filter(!is.na(cambio_num)) %>%  
  # Calcular frecuencias y % por entidad
  group_by(entidad) %>% 
  count(coinciden) %>% 
  mutate(por = n/sum(n)) %>% 
  ungroup() %>% 
  # Filtrar para mantener solo las observaciones con el % de coincidencia
  filter(coinciden == "Coinciden") %>% 
  arrange(-por) %>% 
  print(n = Inf)
