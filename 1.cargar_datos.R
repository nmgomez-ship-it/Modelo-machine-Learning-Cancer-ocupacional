# 01_cargar_datos.R
# Cargar librerías y datos para el análisis de cáncer ocupacional en floricultura

# Limpiar entorno
rm(list = ls())
cat("\014")

# Cargar librerías
library(plm)          # Datos de panel y efectos fijos
library(dplyr)        # Manipulación de datos
library(corrplot)     # Matriz de correlaciones
library(car)          # VIF para multicolinealidad
library(lmtest)       # Test de Hausman, heterocedasticidad
library(sandwich)     # Errores estándar robustos
library(boot)         # Bootstrap
library(mediation)    # Efectos de mediación
library(ggplot2)      # Gráficos
library(reshape2)     # Transformación de datos

# Cargar datos (el usuario debe tener su archivo CSV con las siguientes columnas)
# Columnas requeridas:
# empresa_id, año, ISE, cancer_tasa, productividad, rentabilidad, 
# prima_arl, tamano_log, antiguedad, certificaciones, WCa, GS, QCa, Acc, Y, ROA, Mo

data <- read.csv("datos_floricultura.csv", stringsAsFactors = FALSE)

# Verificar estructura
str(data)
summary(data)

# Configurar panel
pdata <- pdata.frame(data, index = c("empresa_id", "año"))

cat("Datos cargados correctamente. Observaciones:", nrow(data))