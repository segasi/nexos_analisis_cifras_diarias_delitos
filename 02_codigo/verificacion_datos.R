### Cargar paquetes, definir setup y tema de gráficas -----
source("02_codigo/paquetes_setup_tema.R")

### Leer texto la primera hoja -----

# Para hacer las pruebas que describo en "analisis_reportes_homicidios.R, es necesario ajustar la fecha en la última parte del nombre del PDF

texto <- 
  pdf_text(pdf = "01_datos/reportes/gpo_interinstitucional/reporte_gpo_interinstitucional_08092019.pdf")[[1]]

### Extraer dato de las dos columnas ----
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
         # !str_detect(value, "Total"),
         !str_detect(value, "Fuente"),
         !str_detect(value, "Fosas"),
         !str_detect(value, "El día"),
         !str_detect(value, "Dato obtenido"),
         !str_detect(value, "cuerpos ubicados"),
         !str_detect(value, "Abril"),
         !str_detect(value, "Junio"),
         !str_detect(value, "Septiembre"),
         !str_detect(value, "verificados"),
         !str_detect(value, "corresponden")) %>% 
  # Separar valores correspondientes a 2 edos y ponerlo en 2 columnas 
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
  filter(!(col_1 == col_2)) %>%
  print(n = Inf)
         
### Generar tibble con variables separadas -----

# Una columna registra el nombre de la entidad entidad y otra el número de homicidios dolosos del día correspondiente
tabla_datos_separados <- 
  tibble(datos = c(tabla_dos_cols$col_1, tabla_dos_cols$col_2)) %>%
  # Eliminar rengñones con valor NA o con ""
  filter(!str_detect(datos, "Total"),
         datos != "dummy",
         datos != "") %>% 
  # Extrar datos de entidad y número de homicidios
  mutate(entidad = str_replace(datos, " [0-9]*$", ""),
         numero = str_extract_all(datos, "\\(?[0-9,.]+\\)?")) %>% 
  unnest(numero) %>% 
  mutate(numero = as.numeric(numero),
         fecha = make_date(2019, str_sub(string = dia_mes[i], start = 3, end = 4), str_sub(string = dia_mes[i], start = 1, end = 2))) %>% 
  print(n = Inf)
