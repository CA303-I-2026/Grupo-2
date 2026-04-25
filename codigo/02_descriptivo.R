# 02_descriptivo.R
# Análisis descriptivo y exploratorio de los datos
# Autor: Debbie Con, Ashly Garro, Emily Sánchez y Alessandro Umaña 
# Fecha: 22 de abril de 2026

# Se descargan las librerias necesarias:
library(tidyverse)
library(cowplot)
library(ggsci)
library(readr)
#library(scales)

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
View(Accidentes)
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
    title = "Distribución de condición climática",
    x = "Condición climática", 
    y = "Número de accidentes"
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

#----------------------------------------------------------------------------------
# Gráfico de barras: Conteo de accidentes por velocidad límite
#----------------------------------------------------------------------------------
grafico_velocidad <- ggplot(Accidentes, aes(x=as.factor(Speed_limit))) +
  geom_bar(fill = paleta[1], color = "black") +
  labs(
    title = "Conteo de Accidentes por Velocidad Límite",
    x = "Velocidad Límite (mph)",
    y = "Número de Accidentes"
  ) +
  theme_half_open() + 
  estilo_bayesianos()

print(grafico_velocidad)

#----------------------------------------------------------------------------------
# Mapa de coordenadas: Con longitud y latitud se hace un mapa
#----------------------------------------------------------------------------------

Accidentes_mapa <- Accidentes %>%
  #Hay que asegurarse de que existan las coordenadas y filtrar valores erróneos
  filter(!is.na(Longitude), !is.na(Latitude)) %>%
  filter(Longitude != 0, Latitude != 0) 


Mapa_coordenadas <- ggplot(df_mapa_sample, aes(x = Longitude, y = Latitude)) +
  geom_point(alpha = 0.1, size = 0.05, color = paleta[3]) + 
  coord_quickmap() +
  labs(
    title = "Mapa de coordenadas",
    x = "Longitud",
    y = "Latitud"
  ) +
  estilo_bayesianos()

print(Mapa_coordenadas)


#----------------------------------------------------------------------------------
# Cuadro de frecuencias: Intersección en T y obstáculos
#----------------------------------------------------------------------------------


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


#----------------------------------------------------------------------------------
# Histograma de conteo de accidentes por tipo de calle 
#----------------------------------------------------------------------------------


histograma_condiciones <- ggplot(Accidentes, aes(x= Road_Surface_Conditions, fill=Road_Surface_Conditions))+geom_bar()+coord_flip()+
  labs(title = "Conteo de accidentes por condición de carretera",
       x = "Condición de la carretera",
       y = "Número de accidentes") + estilo_bayesianos()+scale_fill_manual(values = paleta)+ scale_y_continuous(labels=label_number())


#----------------------------------------------------------------------------------
# Frecuencia de accidentes por amo dependiendo del tipo de calle 
#----------------------------------------------------------------------------------

histograma_amo_condiciones <- ggplot(Accidentes, aes(y=Year, fill=Road_Surface_Conditions))+geom_bar(position = "dodge")+coord_flip()+
  labs(title = "Conteo de accidentes por condición de carretera",
       x = "Condición de la carretera",
       y = "Número de accidentes") + estilo_bayesianos()+scale_fill_manual(values = paleta)+ scale_y_continuous(labels=label_number())



#----------------------------------------------------------------------------------
# Boxplot de outliers 
#----------------------------------------------------------------------------------


boxplot_num_vehicles<-ggplot(Accidentes, aes(y=Number_of_Vehicles))+geom_boxplot(
  fill = "#4C78A8", outlier.colour = "red" 
  )+coord_flip()+
  labs(title = NULL,
       y = "Número de vehículos") + estilo_bayesianos()+scale_fill_manual(values = paleta)

boxplot_num_casualties<- ggplot(Accidentes, aes(y=Number_of_Casualties))+geom_boxplot(
  fill = "#4C78A8", outlier.colour = "red" 
)+coord_flip()+
  labs(title = NULL,
       y = "Número de víctimas") + estilo_bayesianos()+scale_fill_manual(values = paleta)



boxplot_speed_limit<-ggplot(Accidentes, aes(y=Speed_limit))+geom_boxplot(
  fill = "#4C78A8", outlier.colour = "red" 
)+coord_flip()+
  labs(title = NULL,
       y = "Límite de velocidad") + estilo_bayesianos()+scale_fill_manual(values = paleta)


boxplot_num_vehicles
boxplot_num_casualties
boxplot_speed_limit

plots<- plot_grid(boxplot_num_casualties, boxplot_speed_limit, boxplot_num_vehicles, labels = NULL, ncol = 3)

titulo <- ggdraw() +
  draw_label(
    "Distribución de variables",
    fontface = "bold",
    size = 16
  )
plot_grid(titulo, plots, ncol = 1, rel_heights = c(0.1, 1))

#----------------------------------------------------------------------------------
# Cuadro de variables cuantitativas (min,max,quantiles,media,mediana)
#----------------------------------------------------------------------------------

tabla_num_vehiculo <- data.frame(
  Min = min(Accidentes$Number_of_Vehicles),
  Q1 = as.numeric(quantile(Accidentes$Number_of_Vehicles, 0.25,na.rm = TRUE)),
  Mediana = median(Accidentes$Number_of_Vehicles),
  Q3 = as.numeric(quantile(Accidentes$Number_of_Vehicles, 0.75, na.rm = TRUE)),
  Max = max(Accidentes$Number_of_Vehicles),
  Media = mean(Accidentes$Number_of_Vehicles)
)

tabla_num_casualties <- data.frame(
  Min = min(Accidentes$Number_of_Casualties),
  Q1 = as.numeric(quantile(Accidentes$Number_of_Casualties, 0.25,na.rm = TRUE)),
  Mediana = median(Accidentes$Number_of_Casualties),
  Q3 = as.numeric(quantile(Accidentes$Number_of_Casualties, 0.75, na.rm = TRUE)),
  Max = max(Accidentes$Number_of_Casualties),
  Media = mean(Accidentes$Number_of_Casualties)
)


tabla_num_casualties <- data.frame(
  Min = min(Accidentes$Speed_limit),
  Q1 = as.numeric(quantile(Accidentes$Speed_limit, 0.25,na.rm = TRUE)),
  Mediana = median(Accidentes$Speed_limit),
  Q3 = as.numeric(quantile(Accidentes$Speed_limit, 0.75, na.rm = TRUE)),
  Max = max(Accidentes$Speed_limit),
  Media = mean(Accidentes$Speed_limit)
)









