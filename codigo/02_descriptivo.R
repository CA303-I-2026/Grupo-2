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
View(Accidentes)

#Se crea el histograma de la velocidad límite respecto a la cantidad de accidentes
g10 <- ggplot(Accidentes, aes(x=as.factor(Speed_limit))) +
  geom_bar(fill = paleta[1], color = "black") +
  labs(
    title = "Conteo de Accidentes por Velocidad Límite",
    x = "Velocidad Límite (mph)",
    y = "Número de Accidentes"
  ) +
  theme_half_open() + 
  estilo_bayesianos()

print(g10)

Accidentes_mapa <- Accidentes %>%
  #Hay que asegurarse de que existan las coordenadas y filtrar valores erróneos
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  filter(Longitude != 0, Latitude != 0) 

#Cuadro de coordenadas
p11 <- ggplot(df_mapa_sample, aes(x = Longitude, y = Latitude)) +
  geom_point(alpha = 0.1, size = 0.05, color = paleta[3]) + 
  coord_quickmap() +
  labs(
    title = "Mapa de coordenadas",
    x = "Longitud",
    y = "Latitud"
  ) +
  estilo_bayesianos()

print(p11)

#Se filtra el dataset para realizar lo que se quiere
Accidentes_frecuencias <- Accidentes %>%
  filter(Junction_Detail == "T or staggered junction") %>%
  #Agrupamos y contamos por condición especial (obstáculos)
  group_by(Special_Conditions_at_Site) %>%
  summarise(frecuencia = n()) %>%
  #Eliminar "None" para ver obstáculos reales
  filter(Special_Conditions_at_Site != "None") %>%
  arrange(desc(frecuencia))

#Creación de un cuadro de frecuencias d ela intersección en T y obstáculos
p12 <- ggplot(Accidentes_frecuencias, aes(x = reorder(Special_Conditions_at_Site, frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = paleta[2], color = "black") +
  coord_flip() + #Volteamos para que los nombres de los obstáculos se lean bien
  labs(
    title = "Frecuencia de Obstáculos en Intersecciones en T",
    x = "Tipo de Obstáculo / Condición",
    y = "Número de Accidentes"
  ) +
  estilo_bayesianos()
plot_grid(p12)

