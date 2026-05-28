# 10_calibracion_y_matriz_confusion.R
# Curva de calibración, Brier Score, intercept, slope y matriz de confusión

library(pROC)
library(caret)

# Preparar datos
features <- c("GS", "QCa", "Acc", "Y", "ROA", "Mo")
X <- data[, features]
y <- as.factor(data$WCa)
groups <- data$empresa_id

# Entrenar XGBoost final con GroupKFold y obtener probabilidades
empresas_unicas <- unique(groups)
n_folds <- 5
folds <- createFolds(empresas_unicas, k = n_folds, list = TRUE)

all_true <- c()
all_proba <- c()
all_pred <- c()

for (i in 1:n_folds) {
  test_empresas <- empresas_unicas[folds[[i]]]
  train_idx <- !(groups %in% test_empresas)
  test_idx <- groups %in% test_empresas
  
  X_train <- X[train_idx, ]
  X_test <- X[test_idx, ]
  y_train <- y[train_idx]
  y_test <- y[test_idx]
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)
  model <- xgb.train(params = list(objective = "binary:logistic", 
                                   learning_rate = 0.1, max_depth = 6),
                     data = dtrain, nrounds = 100, verbose = 0)
  
  dtest <- xgb.DMatrix(data = as.matrix(X_test))
  prob <- predict(model, dtest)
  y_pred <- ifelse(prob >= 0.5, 1, 0)
  
  all_true <- c(all_true, as.numeric(y_test) - 1)
  all_proba <- c(all_proba, prob)
  all_pred <- c(all_pred, y_pred)
}

# 1. Matriz de confusión
cm <- confusionMatrix(as.factor(all_pred), as.factor(all_true))
cat("\n=== MATRIZ DE CONFUSIÓN ===\n")
print(cm$table)

# 2. Métricas de calibración
# Brier Score
brier <- mean((all_proba - all_true)^2)
cat("\n=== MÉTRICAS DE CALIBRACIÓN ===\n")
cat("Brier Score:", brier, "\n")

# Curva de calibración
cal_data <- calibration(as.factor(all_true) ~ all_proba, data = data.frame(all_true, all_proba),
                        class = "1", cuts = 10)

# Regresión lineal para intercept y slope
cal_df <- cal_data$data
slope <- coef(lm(Percent ~ midpoint, data = cal_df))[2]
intercept <- coef(lm(Percent ~ midpoint, data = cal_df))[1]
r_squared <- summary(lm(Percent ~ midpoint, data = cal_df))$r.squared

cat("Intercept de calibración:", intercept, "\n")
cat("Slope de calibración:", slope, "\n")
cat("R² de calibración:", r_squared, "\n")

# 3. Gráfico de calibración
ggplot(cal_df, aes(x = midpoint, y = Percent)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(x = "Predicted probability", y = "Observed frequency",
       title = "Calibration Curve - XGBoost") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal()

ggsave("curva_calibracion.png", width = 8, height = 6, dpi = 300)

cat("\nFigura guardada: curva_calibracion.png")