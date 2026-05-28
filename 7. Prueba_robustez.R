# 07_pruebas_robustez.R
# Pruebas de robustez: Hausman, VIF, autocorrelación, heterocedasticidad

# Configurar panel
pdata <- pdata.frame(data, index = c("empresa_id", "año"))

# 1. Test de Hausman (efectos fijos vs aleatorios)
model_fe <- plm(ISE ~ cancer_tasa + productividad + rentabilidad + 
                prima_arl + tamano_log + antiguedad + certificaciones,
                data = pdata, model = "within", effect = "twoways")

model_re <- plm(ISE ~ cancer_tasa + productividad + rentabilidad + 
                prima_arl + tamano_log + antiguedad + certificaciones,
                data = pdata, model = "random", effect = "twoways")

hausman_test <- phtest(model_fe, model_re)
cat("\n=== TEST DE HAUSMAN ===\n")
print(hausman_test)

# 2. VIF para multicolinealidad (usando OLS sin efectos fijos)
model_ols <- lm(ISE ~ cancer_tasa + productividad + rentabilidad + 
                prima_arl + tamano_log + antiguedad + certificaciones,
                data = data)
vif_values <- vif(model_ols)
cat("\n=== FACTORES DE INFLACIÓN DE VARIANZA (VIF) ===\n")
print(vif_values)
cat("VIF promedio:", mean(vif_values), "\n")

# 3. Test de heterocedasticidad (Breusch-Pagan)
bp_test <- bptest(model_ols)
cat("\n=== TEST DE HETEROCEDASTICIDAD (Breusch-Pagan) ===\n")
print(bp_test)

# 4. Test de autocorrelación (Wooldridge)
library(plm)
wooldridge_test <- pwartest(model_fe)
cat("\n=== TEST DE AUTOCORRELACIÓN (Wooldridge) ===\n")
print(wooldridge_test)

cat("\n=== INTERPRETACIÓN ===\n")
cat("Hausman: si p < 0.05 → usar efectos fijos\n")
cat("VIF: si < 5 → sin problemas de multicolinealidad\n")
cat("Breusch-Pagan: si p < 0.05 → hay heterocedasticidad (usar errores robustos)\n")
cat("Wooldridge: si p < 0.05 → hay autocorrelación (usar cluster por empresa)\n")