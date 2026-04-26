# 01_limpieza.R
# Limpieza y preparación de los datos crudos
# Autor: Alessandro Umaña, Emily Sanchez, Debbie Con, Ashly Garro
# Fecha: 26-03-2026

library(tidyverse)
library(readr)
library(dplyr)
Accident_Information <- read_csv("../datos/originales/Accident_Information.csv")
View(Accident_Information)

#Delimitamos la base de datos con los datos a partir del 2010
Accident_Information <- Accident_Information %>%
  filter(Year >= 2010)

#Arerglamos la base de datos
Accident_Information <- Accident_Information %>%
  select(-c(Did_Police_Officer_Attend_Scene_of_Accident,`Local_Authority_(District)`, `Local_Authority_(Highway)`, Police_Force, Location_Easting_OSGR, Location_Northing_OSGR))
View(Accident_Information)

#Resumen 
Accident_Information %>%
  summary()

#Remplazar NAs por la media o moda
#Función para calcular la moda
calcular_moda <- function(x) {
  #Eliminamos valores faltantes para evitar errores
  x <- x[!is.na(x)]
  #Creamos una tabla de frecuencias
  frecuencias <- table(x)
  #Buscamos el valor con la frecuencia máxima
  max_frecuencia <- max(frecuencias)
  #Extraemos el nombre (el valor original) asociado a esa frecuencia
  moda <- names(frecuencias)[frecuencias == max_frecuencia]
  return(moda)
}

#Función para cambiar los NA por la media o moda
procesar_tabla <- function(df, columnas = NULL) {
  #Si no se especifican columnas se usan todas
  if (is.null(columnas)) {
    columnas <- names(df)
  }
  
  #Se crea una copia para no afectar el original
  df_limpio <- df
  
  #Se crea un  ciclo for para ir columna por columna
  for (col in columnas) {
    #Se crea un if para diferenciar si la columna e suna variable cuantitativa o categórica
    if (is.numeric(df_limpio[[col]])) {
      #Se remplaza los NA por la media
      media <- mean(df_limpio[[col]], na.rm = TRUE)
      #Identificamos qué posiciones tienen NA
      posiciones_vacias <- is.na(df_limpio[[col]])
      #Calculamos la media
      media <- mean(df_limpio[[col]], na.rm = TRUE)
      #Solo a esas posiciones les asignamos la media
      df_limpio[[col]][posiciones_vacias] <- media
    } else {
      #Se remplaza los NA por la moda
      valor_moda <- calcular_moda(df_limpio[[col]])[1]
      #Se hace un ifelse para verificar si está vacío o no 
      df_limpio[[col]] <- ifelse(is.na(df_limpio[[col]]), 
                                 valor_moda, 
                                 df_limpio[[col]])
    }
  }
  
  return(df_limpio)
}

Accident_Information <- procesar_tabla(Accident_Information, columnas = c("Latitude", "Longitude", "Speed_limit", "Time"))
Accident_Information %>%
  summary()




Accident_Information_Clean_espanol <- read_csv("Accident_Information_clean.csv")


Accident_Information_Clean_espanol <- Accident_Information_Clean_espanol %>%
  #cambiamos las categorias a espanol
  mutate(
    
    # Condiciones climaticas
    Weather_Conditions = recode(Weather_Conditions,
                                "Other" = "Otro",
                                "Raining no high winds" = "Lluvia sin viento fuerte",
                                "Fine no high winds" = "Despejado sin viento fuerte",
                                "Raining + high winds" = "Lluvia con viento fuerte",
                                "Snowing no high winds" = "Nieve sin viento fuerte",
                                "Fine + high winds" = "Despejado con viento fuerte",
                                "Snowing + high winds" = "Nieve con viento fuerte",
                                "Fog or mist" = "Niebla",
                                "Unknown" = "Desconocido",
                                "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Severidad
    Accident_Severity = recode(Accident_Severity,
                               "Slight" = "Leve",
                               "Serious" = "Grave",
                               "Fatal" = "Fatal"
    ),
    
    # Peligros en la vía
    Carriageway_Hazards = recode(Carriageway_Hazards,
                                 "None" = "Ninguno",
                                 "Pedestrian in carriageway - not injured" = "Peatón en la vía (sin lesión)",
                                 "Other object on road" = "Objeto en la vía",
                                 "Vehicle load on road" = "Carga de vehículo en la vía",
                                 "Any animal in carriageway (except ridden horse)" = "Animal en la vía",
                                 "Previous accident" = "Accidente previo",
                                 "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Control de intersección
    Junction_Control = recode(Junction_Control,
                              "Give way or uncontrolled" = "Ceda el paso / sin control",
                              "Auto traffic signal" = "Semáforo automático",
                              "Authorised person" = "Control por persona autorizada",
                              "Stop sign" = "Señal de alto",
                              "Not at junction or within 20 metres" = "No aplica (no intersección)",
                              "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Tipo de intersección
    Junction_Detail = recode(Junction_Detail,
                             "T or staggered junction" = "Intersección en T",
                             "Mini-roundabout" = "Mini rotonda",
                             "Crossroads" = "Cruce de caminos",
                             "Not at junction or within 20 metres" = "No aplica",
                             "Private drive or entrance" = "Entrada privada",
                             "More than 4 arms (not roundabout)" = "Intersección > 4 vías",
                             "Roundabout" = "Rotonda",
                             "Slip road" = "Vía de incorporación",
                             "Other junction" = "Otra intersección",
                             "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Luz
    Light_Conditions = recode(Light_Conditions,
                              "Daylight" = "Luz del día",
                              "Darkness - lights lit" = "Oscuridad con luces",
                              "Darkness - lights unlit" = "Oscuridad sin luces",
                              "Darkness - lighting unknown" = "Oscuridad (iluminación desconocida)",
                              "Darkness - no lighting" = "Oscuridad sin iluminación",
                              "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Condicion de la carretera 
    Road_Surface_Conditions = recode(Road_Surface_Conditions,
                                     "Dry" = "Seca",
                                     "Wet or damp" = "Húmeda o mojada",
                                     "Frost or ice" = "Escarcha o hielo",
                                     "Snow" = "Nieve",
                                     "Flood over 3cm. deep" = "Inundación > 3cm",
                                     "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Tipo de calle
    Road_Type = recode(Road_Type,
                       "Single carriageway" = "Vía simple",
                       "Dual carriageway" = "Vía doble",
                       "Roundabout" = "Rotonda",
                       "One way street" = "Calle de un solo sentido",
                       "Slip road" = "Vía de incorporación",
                       "Unknown" = "Desconocido",
                       "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Condiciones especiales
    Special_Conditions_at_Site = recode(Special_Conditions_at_Site,
                                        "None" = "Ninguna",
                                        "Roadworks" = "Obras en la vía",
                                        "Auto traffic signal - out" = "Semáforo fuera de servicio",
                                        "Road surface defective" = "Superficie defectuosa",
                                        "Oil or diesel" = "Aceite o diésel",
                                        "Road sign or marking defective or obscured" = "Señalización defectuosa",
                                        "Auto signal part defective" = "Semáforo defectuoso",
                                        "Mud" = "Lodo",
                                        "Data missing or out of range" = "Datos faltantes"
    ),
    
    # Zona
    Urban_or_Rural_Area = recode(Urban_or_Rural_Area,
                                 "Urban" = "Urbano",
                                 "Rural" = "Rural",
                                 "Unallocated" = "No asignado"
    ),
    
    # Día
    Day_of_Week = recode(Day_of_Week,
                         "Monday" = "Lunes",
                         "Tuesday" = "Martes",
                         "Wednesday" = "Miércoles",
                         "Thursday" = "Jueves",
                         "Friday" = "Viernes",
                         "Saturday" = "Sábado",
                         "Sunday" = "Domingo"
    )
  )

















