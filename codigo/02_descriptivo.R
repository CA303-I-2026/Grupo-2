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
paleta <- c(pal_bmj("default")(9), "#4C78A8")

# Se establecen los datos depurados que serán utilizados a lo largo del proyecto
Accidentes <- read.csv("datos/procesados/Accident_Information_clean.csv")

#---------------------------------------------------------------------------
# Cuadro frecuencia: Junction Control
#---------------------------------------------------------------------------

tabla <- table(Accidentes$Junction_Control)

frecuencia_junction_control <- data.frame(
  Categoria = names(tabla),
  Frecuencia = as.numeric(tabla)
)

#-------------------------------------------------------------------------
# Frecuencia cruzada: Condición de calle y severidad
#-------------------------------------------------------------------------
tabla_cruzada <- table(
  Accidentes$Road_Type,
  Accidentes$Accident_Severity
)

severidad_vs_tipocalle <- as.data.frame.matrix(tabla_cruzada)

#-------------------------------------------------------------------------
# Histograma: Frecuencia por Weather_Conditions
#-------------------------------------------------------------------------

ggplot(Accidentes, aes(x = Weather_Conditions, fill = Weather_Conditions)) +
  geom_bar() +
  scale_fill_manual(values = paleta) +
  estilo_bayesianos() +
  labs(
    title = "Distribución de Weather Conditions",
    x = "Condición de calle (clima)", 
    y = "Frecuencia"
  ) +
  coord_flip() +
  theme(legend.position = "none")

#-----------------------------------------------------------------------------
# Gráfico de barras: Cantidad de accidentes según Road_Type y Light_Conditions
#-----------------------------------------------------------------------------

ggplot(Accidentes, aes(x = Road_Type, fill = Light_Conditions)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = paleta) +
  labs(
    title = "Cantidad de accidentes según tipo de calle y condiciones de luz",
    x = "Tipo de calle",
    y = "Cantidad de accidentes"
  ) +
  coord_flip()

#-------------------------------------------------------------------------
# Cuadro: Cantidad de accidentes según Urban_or_Rural_Area 
#-------------------------------------------------------------------------

cuadro_zona <- Accidentes %>%
  count(Urban_or_Rural_Area, name = "Cantidad de accidentes") %>%
  arrange(desc(`Cantidad de accidentes`))

cuadro_zona

#-------------------------------------------------------------------------
# Gráfico de barras: Conteo de accidentes por Accident_Severity
#-------------------------------------------------------------------------

ggplot(Accidentes, aes(x = Accident_Severity, fill = Accident_Severity)) +
  geom_bar() +
  scale_fill_manual(values = paleta) +
  labs(
    title = "Cantidad de accidentes por severidad",
    x = "Severidad",
    y = "Frecuencia"
  ) +
  theme(legend.position = "none")

#----------------------------------------------------------------------------------
# Gráfico de barras: Conteo de accidentes por Accident_Severity agrupado por Year
#----------------------------------------------------------------------------------

ggplot(Accidentes, aes(x = factor(Year), fill = Accident_Severity)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = paleta) +
  labs(
    title = "Conteo de accidentes por severidad y año (2010–2017)",
    x = "Año",
    y = "Cantidad de accidentes"
  )
