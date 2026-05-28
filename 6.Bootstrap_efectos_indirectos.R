# 06_bootstrap_efectos_indirectos.R
# Bootstrap para intervalos de confianza de efectos indirectos

# Función para extraer efectos indirectos
boot_mediation <- function(data, indices) {
  d <- data[indices,]
  pdata <- pdata.frame(d, index = c("empresa_id", "año"))
  
  # Ecuación 1
  m1 <- plm(productividad ~ cancer_tasa + prima_arl + tamano_log + antiguedad + certificaciones,
            data = pdata, model = "within", effect = "twoways")
  
  # Ecuación 2
  m2 <- plm(rentabilidad ~ cancer_tasa + prima_arl + tamano_log + antiguedad + certificaciones,
            data = pdata, model = "within", effect = "twoways")
  
  # Ecuación 3
  m3 <- plm(ISE ~ cancer_tasa + productividad + rentabilidad + prima_arl + tamano_log + antiguedad + certificaciones,
            data = pdata, model = "within", effect = "twoways")
  
  b1 <- coef(m1)["cancer_tasa"]
  g1 <- coef(m2)["cancer_tasa"]
  d1 <- coef(m3)["cancer_tasa"]
  d2 <- coef(m3)["productividad"]
  d3 <- coef(m3)["rentabilidad"]
  
  ind_prod <- b1 * d2
  ind_rent <- g1 * d3
  ind_total <- ind_prod + ind_rent
  direct <- d1
  total <- direct + ind_total
  
  return(c(ind_prod, ind_rent, ind_total, direct, total))
}

# Ejecutar bootstrap con 500 repeticiones
set.seed(42)
boot_results <- boot(data, boot_mediation, R = 500)

# Intervalos de confianza
cat("\n=== INTERVALOS DE CONFIANZA BOOTSTRAP (95%) ===\n")
cat("Efecto indirecto vía productividad:\n")
print(boot.ci(boot_results, index = 1, type = "perc"))
cat("\nEfecto indirecto vía rentabilidad:\n")
print(boot.ci(boot_results, index = 2, type = "perc"))
cat("\nEfecto indirecto total:\n")
print(boot.ci(boot_results, index = 3, type = "perc"))
cat("\nEfecto directo:\n")
print(boot.ci(boot_results, index = 4, type = "perc"))
cat("\nEfecto total:\n")
print(boot.ci(boot_results, index = 5, type = "perc"))