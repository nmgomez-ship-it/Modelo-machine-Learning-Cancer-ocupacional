# 02_construir_ISE.R
# Construir Índice de Sostenibilidad Empresarial (ISE) con 4 dimensiones

# Calcular puntajes para cada dimensión
data <- data %>%
  mutate(
    # 1. Endeudamiento (Pasivo / Patrimonio)
    p_endeudamiento = case_when(
      endeudamiento < 1 ~ 1,
      endeudamiento >= 1 & endeudamiento <= 2 ~ 0.5,
      endeudamiento > 2 ~ 0
    ),
    
    # 2. Rentabilidad (Margen EBITDA)
    p_rentabilidad = case_when(
      rentabilidad > 15 ~ 1,
      rentabilidad >= 5 & rentabilidad <= 15 ~ 0.5,
      rentabilidad < 5 ~ 0
    ),
    
    # 3. Rotación de personal
    p_rotacion = case_when(
      rotacion < 10 ~ 1,
      rotacion >= 10 & rotacion <= 30 ~ 0.5,
      rotacion > 30 ~ 0
    ),
    
    # 4. Reinversión
    p_reinversion = case_when(
      reinversion > 20 ~ 1,
      reinversion >= 5 & reinversion <= 20 ~ 0.5,
      reinversion < 5 ~ 0
    ),
    
    # ISE final (promedio de los 4 puntajes)
    ISE = (p_endeudamiento + p_rentabilidad + p_rotacion + p_reinversion) / 4
  )

# Verificar distribución del ISE
summary(data$ISE)

cat("ISE construido. Media:", mean(data$ISE), "DE:", sd(data$ISE))