# Proyecto CA303 — [Título del Proyecto]

## Integrantes del grupo

| Nombre completo       | Carné | Correo institucional         |
|-----------------------|-------|------------------------------|
|Debbie Con Ortega      |C32250 |debbie.con@ucr.ac.cr          |  
|Ashly Garro Villanueva |C33198 |ashly.garro@ucr.ac.cr         |
|Emily Sánchez Mancía   |C27260 |emily.sanchezmancia@ucr.ac.cr | 
|Alessandro Umaña Vega  |C37963 |alessandro.umana@ucr.ac.cr    |

## Descripción del proyecto

Este proyecto tiene como objetivo analizar la variación en la cantidad de accidentes de tránsito en el Reino Unido durante el periodo 2010–2017, a partir de la identificación de factores del entorno como las condiciones climáticas, el estado de la carretera, la iluminación y el tipo de zona (urbana o rural).

Mediante el uso de herramientas estadísticas, como tablas de contingencia, pruebas de independencia Chi-cuadrado y de homogeneidad, se busca identificar relaciones significativas entre estas variables y la frecuencia de accidentes. El propósito del análisis es comprender mejor las condiciones en las que ocurren los accidentes y aportar evidencia que pueda servir de base para estudios posteriores en el área de seguridad vial.

## Estructura del repositorio

```
proyecto-ca303/
+-- README.md
+-- .gitignore
+-- datos/
|   +-- originales/        # datos crudos, NUNCA se modifican
|   +-- procesados/        # datos limpios listos para análisis
+-- codigo/
|   +-- 01_limpieza.R
|   +-- 02_descriptivo.R
|   +-- 03_modelacion.R
|   +-- funciones/         # funciones auxiliares reutilizables
+-- bitacoras/
|   +-- bitacora_1/
|   |   +-- bitacora_1.tex
|   |   +-- figuras/
|   +-- bitacora_2/
|   +-- bitacora_3/
|   +-- bitacora_4/
+-- fichas/
|   +-- literatura/        # fichas bibliográficas (.md o .tex)
|   +-- resultados/        # fichas de hallazgos (.md o .tex)
+-- referencias/
|   +-- referencias.bib    # archivo BibTeX centralizado
+-- anteproyecto/
|   +-- anteproyecto.tex
+-- proyecto_final/
|   +-- proyecto.tex
|   +-- figuras/
+-- presentacion/
    +-- presentacion.tex
```

## Fuente de datos

El conjunto de datos utilizado en este proyecto corresponde a UK Road Safety: Traffic Accidents and Vehicles, el cual contiene información sobre accidentes de tránsito registrados en el Reino Unido. Estos datos son recopilados y publicados por el Department for Transport del gobierno del Reino Unido y se encuentran disponibles a través de la plataforma Kaggle. La base de datos fue obtenida desde: https://www.kaggle.com/datasets/tsiaras/uk-road-safety-accidents-and-vehicles. La fecha de acceso a los datos fue el 17 de marzo de 2026. En cuanto a su uso, se trata de un conjunto de datos público, sujeto a los términos y condiciones establecidos por Kaggle y por la fuente original.

## Instrucciones de reproducibilidad

<!-- Describa los pasos necesarios para reproducir los resultados: versión de R/Python, paquetes requeridos, orden de ejecución de los scripts, etc. -->

## Avance y bitácoras

| Bitácora | Período | Temas abordados | Estado |
|----------|---------|-----------------|--------|
| Bitácora 1 |       |                 |        |
| Bitácora 2 |       |                 |        |
| Bitácora 3 |       |                 |        |
| Bitácora 4 |       |                 |        |

## Referencias principales

<!-- Liste aquí las referencias más importantes del proyecto (formato APA o BibTeX). El archivo completo se encuentra en `referencias/referencias.bib`. -->
