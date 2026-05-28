# 03_descriptivas_correlaciones.R
# Tablas descriptivas y matriz de correlaciones

# Seleccionar variables para descriptivas
vars_desc <- data[, c("ISE", "cancer_tasa", "productividad", "rentabilidad", 
                      "prima_arl", "tamano_log", "antiguedad", "certificaciones")]

# Tabla de estadísticas descriptivas
descriptivas <- data.frame(
  Variable = names(vars_desc),
  Media = sapply(vars_desc, mean, na.rm = TRUE),
  SD = sapply(vars_desc, sd, na.rm = TRUE),
  Minimo = sapply(vars_desc, min, na.rm = TRUE),
  Maximo = sapply(vars_desc, max, na.rm = TRUE)
)

print(descriptivas)

# Matriz de correlaciones de Pearson
matriz_cor <- cor(vars_desc, use = "complete.obs", method = "pearson")
print(round(matriz_cor, 3))

# Visualizar matriz de correlaciones
corrplot(matriz_cor, method = "number", type = "upper", 
         tl.col = "black", number.cex = 0.8,
         title = "Matriz de Correlaciones - Pearson", mar = c(0,0,1,0))

# Guardar resultados
write.csv(descriptivas, "tabla_descriptivas.csv", row.names = FALSE)
write.csv(round(matriz_cor, 3), "matriz_correlaciones.csv")

cat("Descriptivas y correlaciones guardadas.")