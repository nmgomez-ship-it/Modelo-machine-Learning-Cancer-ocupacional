# Modelo de Machine Learning - Cáncer ocupacional en floricultura

## Descripción
Este repositorio contiene el código utilizado para analizar el impacto del cáncer ocupacional en la sostenibilidad empresarial del sector floricultor en Cundinamarca, Colombia (2014-2024).

## Requisitos
- R versión 4.0 o superior
- Instalar las librerías listadas en `requirements_R.txt`

## Estructura del repositorio
├── 01_cargar_datos.R
├── 02_construir_ISE.R
├── 03_descriptivas_correlaciones.R
├── 04_modelo_mediacion_efectos_fijos.R
├── 05_rezago_ARDL.R
├── 06_bootstrap_efectos_indirectos.R
├── 07_pruebas_robustez.R
├── 08_ml_groupkfold.R
├── 09_metricas_bootstrap_ml.R
├── 10_calibracion_y_matriz_confusion.R
├── 11_figuras.R
├── requirements_R.txt
└── README.md

## Ejecución
1. Abrir RStudio
2. Ejecutar los scripts en orden numérico:
   ```r
   source("01_cargar_datos.R")
   source("02_construir_ISE.R")
   source("03_descriptivas_correlaciones.R")
   source("04_modelo_mediacion_efectos_fijos.R")
   source("05_rezago_ARDL.R")
   source("06_bootstrap_efectos_indirectos.R")
   source("07_pruebas_robustez.R")
   source("08_ml_groupkfold.R")
   source("09_metricas_bootstrap_ml.R")
   source("10_calibracion_y_matriz_confusion.R")
   source("11_figuras.R")
## Datos
Los datos crudos no pueden ser compartidos por acuerdos de confidencialidad con las empresas y trabajadores participantes. El código requiere un archivo CSV con las columnas especificadas en los scripts.

## Contacto
Para preguntas: nigomez86@unisalle.edu.co