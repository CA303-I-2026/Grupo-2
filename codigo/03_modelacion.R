# 03_modelacion.R
# Modelación estadística
# Autor: Debbie Con, Ashly Garro, Emily Sánchez y Alessandro Umaña
# Fecha: 11 de junio del 2026


# Se descargan las librerias necesarias:
library(tidyverse)
library(cowplot)
library(ggsci)
library(readr)
library(scales)
library(lubridate)
library(ggridges)
library(DescTools)
library(reshape2)


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


#Accident_Information_Clean_espanol <- read_csv("datos/procesados/Accident_Information_Clean_espanol.csv")
Accident_Information_Clean_espanol <-read.csv("C:/Users/aless/OneDrive/Escritorio/Grupo-2/datos/procesados/Accident_Information_Clean_espanol.csv")
View(Accident_Information_Clean_espanol)

#volvemos a cargar la estética de los gráficos
#Se define el tema que se utilizará para la creación de gráficos
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

#Se fija el tema para su uso durante todo el proyecto
theme_set(estilo_bayesianos())

#Se escoge la paleta BMJ del paquete ggsci (9 colores)
paleta <- c(pal_bmj("default")(9), "#4C78A8")

#----------------------------------------------------------------------------------
# 1. Tabla de contingencia de superficie de la vía y condición de la vía
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_luz_superficie <- table(Accident_Information_Clean_espanol$Road_Surface_Conditions, Accident_Information_Clean_espanol$Light_Conditions)
tabla_luz_superficie <- tabla_luz_superficie[!rownames(tabla_luz_superficie) %in% c("Datos faltantes"),
                                             !colnames(tabla_luz_superficie) %in% c("Datos faltantes", "Desconocido")]
#Se imprime la tabla
print("=== CASO 1: SUPERFICIE Y LUZ ===")
print(tabla_luz_superficie)

#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba1 <- chisq.test(tabla_luz_superficie)
print(prueba1)


cat("\n--- V de Crámer ---\n")
prueba1.1<-CramerV(tabla_luz_superficie)
print(prueba1.1)


cat("\n--- Residuos Estandarizados ---\n")
prueba1.2 <- prueba1$stdres
res_prueba1.2 <- round(prueba1.2,2)
pos_prueba1.2 <- which(abs(prueba1.2) > 2, arr.ind = TRUE)

tabla_prueba1.2 <-data.frame(
  Superficie_carretera = rownames(res_prueba1.2)[pos_prueba1.2[,1]],
  Luminocidad = colnames(res_prueba1.2)[pos_prueba1.2[,2]],
  Residuo = res_prueba1.2[pos_prueba1.2]
)

tabla_prueba1.2 <- tabla_prueba1.2[order(tabla_prueba1.2$Residuo, decreasing = TRUE),]
print(tabla_prueba1.2)

# --------- Heatmap de residuos ----------------------- #
residuos_luz_superficie <- as.data.frame(as.table(prueba1.2))
colnames(residuos_luz_superficie) <- c( "Superficie", "Luminocidad", "Residuo")

ggplot(residuos_luz_superficie, aes(x = Superficie, y = Luminocidad, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) +
  scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: Superficie de la vía y condición de luz",
    fill = "Residuo",
    x = "Superficie de la vía",
    y = "Condición de luz"
  )  + estilo_bayesianos() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#----------------------------------------------------------------------------------
# 2. Tabla de contingencia de intersecciones en T y obstáculos en la vía
#----------------------------------------------------------------------------------

tabla_interseccion_obstaculos <- Accident_Information_Clean_espanol %>%
  # Se filtra para excluir los "Datos faltantes" en ambas variables
  filter(
    Junction_Detail != "Datos faltantes",
    Special_Conditions_at_Site != "Datos faltantes"
  ) %>%
  
  # Se crean las nuevas variables agrupadas
  mutate(
    # Agrupación para Junction_Detail
    Tipo_Cruce = case_when(
      Junction_Detail %in% c("Intersección en T", "Intersección > 4 vías", "Vía de incorporación", "Otra intersección", "Cruce de caminos", "Entrada privada") ~ "Intersecciones",
      Junction_Detail %in% c("Mini rotonda", "Rotonda") ~ "Rotondas",
      Junction_Detail == "No aplica" ~ "No aplica"
    ),
    
    # Agrupación para Special_Conditions_at_Site
    Condicion_Sitio = case_when(
      Special_Conditions_at_Site %in% c("Semáforo fuera de servicio", "Señalización defectuosa", "Semáforo defectuoso") ~ "Control vial defectuoso",
      Special_Conditions_at_Site == "Ninguna" ~ "Ninguna",
      Special_Conditions_at_Site == "Obras en la vía" ~ "Obras en la vía",
      Special_Conditions_at_Site %in% c("Lodo", "Aceite o diésel") ~ "Superficie resbaladiza",
      Special_Conditions_at_Site == "Superficie defectuosa" ~ "Superficie defectuosa"
    )
  ) %>%
  
  # Se seleccionan las nuevas variables calculadas
  select(Tipo_Cruce, Condicion_Sitio) %>%
  
  # Se genera la tabla de frecuencia cruzada
  table()



#imprimir la tabla 
print("=== CASO 2: INTERSECCIÓN EN T Y OBSTÁCULOS ===")
print(tabla_interseccion_obstaculos)

#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba2 <- chisq.test(tabla_interseccion_obstaculos)
print(prueba2)

# Prueba V de Crámer
cat("\n--- V de Crámer ---\n")
prueba2.1<-cramer.v(tabla_interseccion_obstaculos)
print(prueba2.1)

# Prueba residuos Estandarizados 
cat("\n--- Residuos Estandarizados ---\n")
prueba2.2 <- prueba2$stdres
res_prueba2.2 <- round(prueba2.2,2)
pos_prueba2.2 <- which(abs(prueba2.2) > 2, arr.ind = TRUE)

tabla_prueba2.2 <-data.frame(
  Detalles_de_interseccion = rownames(res_prueba2.2)[pos_prueba2.2[,1]],
  Condiciones_especiales = colnames(res_prueba2.2)[pos_prueba2.2[,2]],
  Residuo = res_prueba2.2[pos_prueba2.2]
)

tabla_prueba2.2 <- tabla_prueba2.2[order(tabla_prueba2.2$Residuo, decreasing = TRUE),]
print(tabla_prueba2.2)

print(tabla_prueba2.2[tabla_prueba2.2$Residuo > 2, ])

# --------- Heatmap de residuos ----------------------- #
residuos_interseccion_obstaculos <- as.data.frame(as.table(prueba2.2))
colnames(residuos_interseccion_obstaculos) <- c( "Interseccion", "Obstaculos", "Residuo")

ggplot(residuos_interseccion_obstaculos, aes(x = Interseccion, y = Obstaculos, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) +
  scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: Detalles de intersección y condiciones especiales de la vía",
    fill = "Residuo",
    x = "Detalles de intersección",
    y = "Condiciones especiales de la vía"
  )  + estilo_bayesianos() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )



#----------------------------------------------------------------------------------
# 3. Tabla de contingencia de condición climática y obstáculos en la vía
#----------------------------------------------------------------------------------

#Se agrupan las categorías para evitar repeticiones

Accident_Information_Clean_espanol <- Accident_Information_Clean_espanol %>% 
  mutate(obstaculo_agrupado = case_when(
    Special_Conditions_at_Site %in% c("Semáforo defectuoso", "Semáforo fuera de servicio", "Señalización defectuosa")  ~"Control vial defectuoso",
    Special_Conditions_at_Site %in% c("Lodo", "Aceite o diésel") ~ "Superficie resbaladiza",
    TRUE ~ as.character(Special_Conditions_at_Site)
  )
  )
Accident_Information_Clean_espanol <- Accident_Information_Clean_espanol %>%
  mutate(clima_agrupado = case_when(
    Weather_Conditions %in% c("Despejado con viento fuerte", "Despejado sin viento fuerte") ~ "Despejado",
    Weather_Conditions %in% c("Lluvia con viento fuerte", "Lluvia sin viento fuerte") ~ "Lluvia",
    Weather_Conditions %in% c("Nieve con viento fuerte","Nieve sin viento fuerte") ~ "Nieve",
TRUE ~ as.character(Weather_Conditions)))

#se crea la tabla
tabla_clima_obstaculos2 <- table(Accident_Information_Clean_espanol$clima_agrupado, Accident_Information_Clean_espanol$obstaculo_agrupado)
tabla_clima_obstaculos2 <- tabla_clima_obstaculos2[!rownames(tabla_clima_obstaculos2) %in% c("Datos faltantes", "Desconocido"), 
  !colnames(tabla_clima_obstaculos2) %in% c("Datos faltantes", "Desconocido")]


#Se imprime la tabla
print("=== CASO 3: CLIMA Y OBSTÁCULO ===")
print(tabla_clima_obstaculos2)

#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba3 <- chisq.test(tabla_clima_obstaculos2)
print(prueba3)

cat("\n--- V de Crámer ---\n")
prueba3.1 <- CramerV(tabla_clima_obstaculos2)
print(prueba3.1)

cat("\n--- Residuos Estandarizados ---\n")
prueba3.2 <- prueba3$stdres
print(prueba3.2)


# --------- Heatmap de residuos ----------------------- #
residuos_clima_obstaculo <- as.data.frame(as.table(prueba3.2))
colnames(residuos_clima_obstaculo) <- c( "Clima", "Obstaculo", "Residuo")

heatmap_clima_obs <- ggplot(residuos_clima_obstaculo, aes(x = Clima, y = Obstaculo,fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) +
  scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: \nCondición climática y obstáculos en la vía",
    fill = "Residuo",
    x = "Clima",
    y = "Obstáculo en vía"
  )  + estilo_bayesianos() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

print(heatmap_clima_obs)

#----------------------------------------------------------------------------------
# 4. Tabla de contingencia de condición de luz y el tipo de carretera
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_luz_carretera <- table(Accident_Information_Clean_espanol$Light_Conditions, Accident_Information_Clean_espanol$Road_Type)
tabla_luz_carretera<- tabla_luz_carretera[!rownames(tabla_luz_carretera) %in% c("Datos faltantes"),
                                          !colnames(tabla_luz_carretera) %in% c("Datos faltantes", "Desconocido")]

#Se imprime la tabla
print("=== CASO 4: LUZ Y TIPO DE CARRETERA ===")
print(tabla_luz_carretera)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba4 <- chisq.test(tabla_luz_carretera)
print(prueba4)


cat("\n--- V de Crámer ---\n")
prueba4.1<-CramerV(tabla_luz_carretera)
print(prueba4.1)


cat("\n--- Residuos Estandarizados ---\n")
prueba4.2 <- prueba4$stdres
res_prueba4.2 <- round(prueba4.2,2)
pos_prueba4.2 <- which(abs(prueba4.2) > 2, arr.ind = TRUE)

tabla_prueba4.2 <-data.frame(
  Iluminacion = rownames(res_prueba4.2)[pos_prueba4.2[,1]],
  Tipo_Carretera = colnames(res_prueba4.2)[pos_prueba4.2[,2]],
  Residuo = res_prueba4.2[pos_prueba4.2]
)

tabla_prueba4.2 <- tabla_prueba4.2[order(tabla_prueba4.2$Residuo, decreasing = TRUE),]
print(tabla_prueba4.2)

# --------- Heatmap de residuos ----------------------- #
ggplot(tabla_prueba4.2, aes(x = Iluminacion, y = Tipo_Carretera, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) + 
  scale_x_discrete(labels = c(
  "Luz del día" = "Luz\ndel día",
  "Oscuridad (iluminación desconocida)" = "Oscuridad\n(iluminación desconocida)",
  "Oscuridad con luces" = "Oscuridad con\nluces",
  "Oscuridad sin iluminación" = "Oscuridad sin\niluminación",
  "Oscuridad sin luces" = "Oscuridad sin\nluces"
))+
  scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: \nCondiciones de iluminación y vía",
    fill = "Residuo",
    x = "Tipo de vía",
    y = "Iluminación"
  ) + theme_cowplot() + estilo_bayesianos() +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 10
    ),
    plot.title = element_text(hjust = 0.5)
  )

#----------------------------------------------------------------------------------
# 5. Tabla de contingencia de si el área es rural o urban y la hora
#----------------------------------------------------------------------------------

#se resumen las horas en franjas horarias
hora <- as.numeric(substr(Accident_Information_Clean_espanol$Time,1,2))
Accident_Information_Clean_espanol$franja<-cut(
  hora,
  breaks = c(-1,5,11,17,23),
  labels = c("Madrugada", "Mañana", "Tarde", "Noche")
)


#se crea la tabla
tabla_area_hora <- table(Accident_Information_Clean_espanol$Urban_or_Rural_Area, Accident_Information_Clean_espanol$franja)
tabla_area_hora<- tabla_area_hora[!rownames(tabla_area_hora) %in% c("No asignado"),
                                          !colnames(tabla_area_hora) %in% c("No asignado")]

#Se imprime la tabla
print("=== CASO 5: ÁREA Y TIEMPO ===")
print(tabla_area_hora)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba5 <- chisq.test(tabla_area_hora)
print(prueba5)


cat("\n--- V de Crámer ---\n")
prueba5.1<-CramerV(tabla_area_hora)
print(prueba5.1)


cat("\n--- Residuos Estandarizados ---\n")
prueba5.2 <- prueba5$stdres
res_prueba5.2 <- round(prueba5.2,2)
pos_prueba5.2 <- which(abs(prueba5.2) > 2, arr.ind = TRUE)

tabla_prueba5.2 <-data.frame(
  Zona = rownames(res_prueba5.2)[pos_prueba5.2[,1]],
  Hora = colnames(res_prueba5.2)[pos_prueba5.2[,2]],
  Residuo = res_prueba5.2[pos_prueba5.2]
)

tabla_prueba5.2 <- tabla_prueba5.2[order(tabla_prueba5.2$Residuo, decreasing = TRUE),]
print(tabla_prueba5.2)

# --------- Heatmap de residuos ----------------------- #
ggplot(tabla_prueba5.2, aes(x = Zona, y = Hora, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) + scale_y_discrete(
    limits = c("Madrugada", "Noche", "Tarde", "Mañana")
  ) + scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: \nZona y hora",
    fill = "Residuo",
    x = "Zona",
    y = "Hora"
  ) + theme_cowplot() + estilo_bayesianos() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#----------------------------------------------------------------------------------
# 6. Tabla de contingencia de condición de la superficie y el tipo de carretera
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_superficie_carretera <- table(Accident_Information_Clean_espanol$Road_Surface_Conditions, Accident_Information_Clean_espanol$Road_Type)
tabla_superficie_carretera<- tabla_superficie_carretera[!rownames(tabla_superficie_carretera) %in% c("Datos faltantes"),
                                                        !colnames(tabla_superficie_carretera) %in% c("Datos faltantes", "Desconocido")]
#Se imprime la tabla
print("=== CASO 6: CONDICION DE SUPERFICIE Y TIPO DE CARRETERA ===")
print(tabla_superficie_carretera)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba6 <- chisq.test(tabla_superficie_carretera)
print(prueba6)

# Prueba V de Crámer 
cat("\n--- V de Crámer ---\n")
prueba6.1<-cramer.v(tabla_superficie_carretera)
print(prueba6.1)

# Prueba Residuos Estandarizados 
cat("\n--- Residuos Estandarizados ---\n")
prueba6.2 <- prueba6$stdres
res_prueba6.2 <- round(prueba6.2,2)
pos_prueba6.2 <- which(abs(prueba6.2) > 2, arr.ind = TRUE)

tabla_prueba6.2 <-data.frame(
  Condiciones_de_carretera = rownames(res_prueba6.2)[pos_prueba6.2[,1]],
  Tipo_de_carretera = colnames(res_prueba6.2)[pos_prueba6.2[,2]],
  Residuo = res_prueba6.2[pos_prueba6.2]
)


# --------- Heatmap de residuos ----------------------- #
residuos_superficie_carretera <- as.data.frame(as.table(prueba6.2))
colnames(residuos_superficie_carretera) <- c( "superficie", "carretera", "Residuo")

ggplot(residuos_superficie_carretera, aes(x = superficie, y = carretera, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) +
  scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: Condición de superficie de carretera y tipo de vía",
    fill = "Residuo",
    x = "Condición de superficie de carretera",
    y = "Tipo de vía"
  )  + estilo_bayesianos() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#----------------------------------------------------------------------------------
# 7. Tabla de contingencia de dia de la semana y la zona
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_dia_zona <- table(Accident_Information_Clean_espanol$Day_of_Week, Accident_Information_Clean_espanol$Urban_or_Rural_Area)
tabla_dia_zona <- tabla_dia_zona[,!colnames(tabla_dia_zona) %in% c("No asignado")]

#Se imprime la tabla
print("=== CASO 7: DÍA DE LA SEMANA Y ZONA ===")
print(tabla_dia_zona)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba7 <- chisq.test(tabla_dia_zona)
print(prueba7)

cat("\n--- V de Crámer ---\n")
prueba7.1<-CramerV(tabla_dia_zona)
print(prueba7.1)


cat("\n--- Residuos Estandarizados ---\n")
prueba7.2 <- prueba7$stdres
res_prueba7.2 <- round(prueba7.2,2)
pos_prueba7.2 <- which(abs(prueba7.2) > 2, arr.ind = TRUE)


tabla_prueba7.2 <-data.frame(
  Dia_semana = rownames(res_prueba7.2)[pos_prueba7.2[,1]],
  Zona = colnames(res_prueba7.2)[pos_prueba7.2[,2]],
  Residuo = res_prueba7.2[pos_prueba7.2]
)

tabla_prueba7.2 <- tabla_prueba7.2[order(tabla_prueba7.2$Residuo, decreasing = TRUE),]
print(tabla_prueba7.2)

# --------- Heatmap de residuos ----------------------- #
residuos_dia_zona <- as.data.frame(as.table(prueba7.2))
colnames(residuos_dia_zona) <- c( "Día", "Zona", "Residuo")

#volvemos a cargar la estética de los gráficos
#Se define el tema que se utilizará para la creación de gráficos
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

#Se fija el tema para su uso durante todo el proyecto
theme_set(estilo_bayesianos())

#Se escoge la paleta BMJ del paquete ggsci (9 colores)
paleta <- c(pal_bmj("default")(9), "#4C78A8")

ggplot(residuos_dia_zona, aes(x = Día, y = Zona, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Residuo,1)), size = 3.8) +
  scale_fill_gradient2(low = paleta[2],mid = "white",high = paleta[3], midpoint = 0) +
  labs(
    title = "Residuos estandarizados: Dia de la semana y zona",
    fill = "Residuo",
    x = "Día de la semana",
    y = "Zona"
  )  + estilo_bayesianos() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#----------------------------------------------------------------------------------
# 8. Tabla de contingencia de superficie de la vía y el límite de velocidad
#----------------------------------------------------------------------------------
Accident_Information_Clean_espanol <- Accident_Information_Clean_espanol %>%
  mutate(
    velocidad_agrupada = case_when(
      Speed_limit %in% c(0, 38.2791280970515) ~ "Desconocido",
      Speed_limit %in% c(10, 15, 20, 30) ~ "Baja",
      Speed_limit %in% c(40, 50) ~ "Media",
      Speed_limit %in% c(60, 70) ~ "Alta",
      TRUE ~ as.character(Speed_limit)
    )
  )

#se crea la tabla
tabla_superficie_limite <- table(Accident_Information_Clean_espanol$Road_Surface_Conditions, Accident_Information_Clean_espanol$velocidad_agrupada)
tabla_superficie_limite <- tabla_superficie_limite[!rownames(tabla_superficie_limite) %in% c("Datos faltantes", "Desconocido"), 
  !colnames(tabla_superficie_limite) %in% c("Datos faltantes", "Desconocido")]

#Se imprime la tabla
print("=== CASO 8: SUPERFICIE CONDICIÓN DE SUPERFICIE Y LÍMITE DE VELOCIDAD ===")
print(tabla_superficie_limite)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba8 <- chisq.test(tabla_superficie_limite)
print(prueba8)

cat("\n--- V de Crámer ---\n")
prueba8.1 <-CramerV(tabla_superficie_limite)
print(prueba8.1)

cat("\n--- Residuos Estandarizados ---\n")
prueba8.2 <- prueba8$stdres
print(prueba8.2)

residuos_superficie_velocidad <- as.data.frame(as.table(prueba8.2))

colnames(residuos_superficie_velocidad) <- c(
  "Superficie",
  "Velocidad",
  "Residuo"
)

heatmap_sup_vel <- ggplot(residuos_superficie_velocidad, aes(x = Velocidad, y = Superficie, fill = Residuo)) +
  geom_tile(color = "white") +
  geom_text(
    aes(label = round(Residuo, 1)),
    size = 4
  ) +
  scale_fill_gradient2(
    low = paleta[2],
    mid = "white",
    high = paleta[3],
    midpoint = 0
  ) +
  labs(
    title = "Residuos estandarizados: \nCondición de la superficie vial y límite de velocidad",
    fill = "Residuo"
  ) +
  estilo_bayesianos() +
  theme(
    plot.title = element_text(
      hjust = 0.5
    )
  )

print(heatmap_sup_vel)