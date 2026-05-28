# 04_modelo_mediacion_efectos_fijos.R
# Modelo de mediación con efectos fijos de empresa y año

# Configurar panel
pdata <- pdata.frame(data, index = c("empresa_id", "año"))

# Ecuación 1: Efecto del cáncer sobre productividad (mediador 1)
model_prod <- plm(productividad ~ cancer_tasa + prima_arl + tamano_log + 
                  antiguedad + certificaciones,
                  data = pdata, model = "within", effect = "twoways")

# Ecuación 2: Efecto del cáncer sobre rentabilidad (mediador 2)
model_rent <- plm(rentabilidad ~ cancer_tasa + prima_arl + tamano_log + 
                  antiguedad + certificaciones,
                  data = pdata, model = "within", effect = "twoways")

# Ecuación 3: Efecto directo e indirecto sobre ISE
model_ise <- plm(ISE ~ cancer_tasa + productividad + rentabilidad + 
                 prima_arl + tamano_log + antiguedad + certificaciones,
                 data = pdata, model = "within", effect = "twoways")

# Mostrar resultados
summary(model_prod)
summary(model_rent)
summary(model_ise)

# Errores estándar robustos cluster por empresa
coeftest(model_prod, vcov = vcovHC(model_prod, type = "HC1", cluster = "group"))
coeftest(model_rent, vcov = vcovHC(model_rent, type = "HC1", cluster = "group"))
coeftest(model_ise, vcov = vcovHC(model_ise, type = "HC1", cluster = "group"))

# Extraer coeficientes para efectos indirectos
b1 <- coef(model_prod)["cancer_tasa"]
g1 <- coef(model_rent)["cancer_tasa"]
d1 <- coef(model_ise)["cancer_tasa"]
d2 <- coef(model_ise)["productividad"]
d3 <- coef(model_ise)["rentabilidad"]

# Calcular efectos indirectos
efecto_ind_prod <- b1 * d2
efecto_ind_rent <- g1 * d3
efecto_ind_total <- efecto_ind_prod + efecto_ind_rent
efecto_total <- d1 + efecto_ind_total

cat("\n=== EFECTOS DE MEDIACIÓN ===\n")
cat("Efecto indirecto vía productividad:", efecto_ind_prod, "\n")
cat("Efecto indirecto vía rentabilidad:", efecto_ind_rent, "\n")
cat("Efecto indirecto total:", efecto_ind_total, "\n")
cat("Efecto directo:", d1, "\n")
cat("Efecto total:", efecto_total, "\n")

# Porcentajes
cat("\n=== PORCENTAJES DEL EFECTO TOTAL ===\n")
cat("Efecto directo:", (d1 / efecto_total) * 100, "%\n")
cat("Efecto indirecto vía productividad:", (efecto_ind_prod / efecto_total) * 100, "%\n")
cat("Efecto indirecto vía rentabilidad:", (efecto_ind_rent / efecto_total) * 100, "%\n")