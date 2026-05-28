# 05_rezago_ARDL.R
# Modelo dinámico con rezago del ISE

# Ordenar datos por empresa y año
data <- data %>%
  arrange(empresa_id, año)

# Configurar panel
pdata <- pdata.frame(data, index = c("empresa_id", "año"))

# Crear rezago del ISE
pdata$ISE_lag1 <- lag(pdata$ISE, 1)

# Modelo dinámico con efectos fijos
modelo_dinamico <- plm(ISE ~ ISE_lag1 + cancer_tasa + productividad + rentabilidad + 
                       prima_arl + tamano_log + antiguedad + certificaciones,
                       data = pdata, model = "within", effect = "twoways")

# Mostrar resultados
summary(modelo_dinamico)

# Errores estándar robustos cluster por empresa
coeftest(modelo_dinamico, vcov = vcovHC(modelo_dinamico, type = "HC1", cluster = "group"))

# Comparación con modelo estático
cat("\n=== COMPARACIÓN CON MODELO ESTÁTICO ===\n")
cat("Coeficiente cáncer en modelo estático:", d1, "\n")
cat("Coeficiente cáncer en modelo dinámico:", coef(modelo_dinamico)["cancer_tasa"], "\n")