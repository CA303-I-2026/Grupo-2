# 01_limpieza.R
# Limpieza y preparación de los datos crudos
# Autor: Alessandro Umaña, Emily Sanchez, Debbie Con, Ashly Garro
# Fecha: 26-03-2026

library(tidyverse)

informacion_accidentes <- read_csv("datos/originales/Accident_Information.csv")
View(informacion_accidentes)

accidentes_filtrados <- informacion_accidentes %>%
  filter(Year >= 2010)