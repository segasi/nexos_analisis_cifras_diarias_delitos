### Paquetes ----
library(pacman)
p_load(cowplot, downloader, extrafont, forecast, ggrepel, ggtext, janitor, lubridate, pdftools, psych, readxl, scales, seasonal, sweep, tidyquant, tidyverse, timetk, treemapify, zoo) 

# Importart fuentes
font_import()

### Setup general ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen = 9999)
theme_set(theme_gray())

### Definir tema de gráficas ----
tema <-  
  theme_minimal() +
  theme(text = element_text(family = "System Font", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family = "Lato Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "System Font"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Lato Bold"),
        legend.text = element_text(size = 14, family = "System Font"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "System Font"),
        axis.text = element_text(size = 16, face = "bold", family = "System Font"))
