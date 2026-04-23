# 02_descriptivo.R
# Análisis descriptivo y exploratorio de los datos
# Autor: Debbie Con, Ashly Garro, Emily Sánchez y Alessandro Umaña 
# Fecha: 22 de abril de 2026

# Se descargan las librerias necesarias:
library(tidyverse)
library(cowplot)
library(ggsci)
library(readr)

# Se define el tema que se utilizará para la creación de gráficos
estilo_bayesianos <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 11),
      panel.grid = element_blank(),   # sin grilla
      axis.line = element_line(color = "black"),
      legend.position = "top",
      legend.title = element_blank(),
      plot.background = element_blank()
    )
}

# Se fija el tema para su uso durante todo el proyecto
theme_set(estilo_bayesianos())

# Se escoge la paleta BMJ del paquete ggsci (9 colores)
paleta <- pal_bmj("default")(9)

# Se establecen los datos depurados que serán utilizados a lo largo del proyecto
Accidentes <- read.csv("datos/procesados/Accident_Information_clean.csv")

