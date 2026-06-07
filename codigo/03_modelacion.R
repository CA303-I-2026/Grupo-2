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

Accident_Information_Clean_espanol <- read_csv("datos/procesados/Accident_Information_Clean_espanol.csv")
View(Accident_Information_Clean_espanol)


#----------------------------------------------------------------------------------
# 1. Tabla de contingencia de superficie de la vía y condición de la vía
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_luz_superficie <- table(Accident_Information_Clean_espanol$Road_Surface_Conditions, Accident_Information_Clean_espanol$Light_Conditions)

#Se imprime la tabla
print("=== CASO 1: SUPERFICIE Y LUZ ===")
print(tabla_luz_superficie)

#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba1 <- chisq.test(tabla_luz_superficie)
print(prueba1)


#----------------------------------------------------------------------------------
# 2. Tabla de contingencia de intersecciones en T y obstáculos en la vía
#----------------------------------------------------------------------------------

#Se filtra el dataset para realizar lo que se quiere

tabla_interseccion_obstaculos <- Accident_Information_Clean_espanol %>%
  
  filter(Special_Conditions_at_Site != "Ninguna") %>%
  
  #se crea una nueva variable que clasifique si es Intersección en T o NO
  mutate(Tipo_Cruce = ifelse(Junction_Detail == "Intersección en T", 
                             "Intersección en T", 
                             "Otros detalles de vía")) %>%
  select(Tipo_Cruce, Special_Conditions_at_Site) %>%
  table()

#imprimir la tabla 
print("=== CASO 2: INTERSECCIÓN EN T Y OBSTÁCULOS ===")
print(tabla_interseccion_obstaculos)

#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba2 <- chisq.test(tabla_interseccion_obstaculos)
print(prueba2)

#----------------------------------------------------------------------------------
# 3. Tabla de contingencia de condición climática y obstáculos en la vía
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_clima_obstaculos <- table(Accident_Information_Clean_espanol$Weather_Conditions, Accident_Information_Clean_espanol$Special_Conditions_at_Site)

#Se imprime la tabla
print("=== CASO 3: CLIMA Y OBSTÁCULO ===")
print(tabla_clima_obstaculos)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba3 <- chisq.test(tabla_clima_obstaculos)
print(prueba3)

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



#----------------------------------------------------------------------------------
# 5. Tabla de contingencia de si el área es rural o urban y la hora
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_area_hora <- table(Accident_Information_Clean_espanol$Urban_or_Rural_Area, Accident_Information_Clean_espanol$Time)

#Se imprime la tabla
print("=== CASO 5: ÁREA Y TIEMPO ===")
print(tabla_area_hora)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba5 <- chisq.test(tabla_area_hora)
print(prueba5)

#----------------------------------------------------------------------------------
# 6. Tabla de contingencia de condición de la superficie y el tipo de carretera
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_superficie_carretera <- table(Accident_Information_Clean_espanol$Road_Surface_Conditions, Accident_Information_Clean_espanol$Road_Type)

#Se imprime la tabla
print("=== CASO 6: CONDICION DE SUPERFICIE Y TIPO DE CARRETERA ===")
print(tabla_superficie_carretera)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba6 <- chisq.test(tabla_superficie_carretera)
print(prueba6)

#----------------------------------------------------------------------------------
# 7. Tabla de contingencia de dia de la semana y la zona
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_dia_zona <- table(Accident_Information_Clean_espanol$Day_of_Week, Accident_Information_Clean_espanol$Urban_or_Rural_Area)

#Se imprime la tabla
print("=== CASO 7: DÍA DE LA SEMANA Y ZONA ===")
print(tabla_dia_zona)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba7 <- chisq.test(tabla_dia_zona)
print(prueba7)

#----------------------------------------------------------------------------------
# 8. Tabla de contingencia de superficie de la vía y el límite de velocidad
#----------------------------------------------------------------------------------

#se crea la tabla
tabla_superficie_limite <- table(Accident_Information_Clean_espanol$Road_Surface_Conditions, Accident_Information_Clean_espanol$Speed_limit)

#Se imprime la tabla
print("=== CASO 8: SUPERFICIE CONDICIÓN DE SUPERFICIE Y LÍMITE DE VELOCIDAD ===")
print(tabla_superficie_limite)


#Prueba de Independencia Chi-cuadrado
cat("\n--- Prueba Chi-cuadrado de Independencia ---\n")
prueba8 <- chisq.test(tabla_superficie_limite)
print(prueba8)
