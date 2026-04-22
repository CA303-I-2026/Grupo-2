# 01_limpieza.R
# Limpieza y preparación de los datos crudos
# Autor: Alessandro Umaña, Emily Sanchez, Debbie Con, Ashly Garro
# Fecha: 26-03-2026

library(tidyverse)
library(readr)
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