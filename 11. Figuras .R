# 11_figuras.R
# Generar figuras: curvas ROC, histogramas, etc.

library(ggplot2)
library(pROC)

# Preparar datos para ROC
features <- c("GS", "QCa", "Acc", "Y", "ROA", "Mo")
X <- data[, features]
y <- data$WCa
groups <- data$empresa_id

# Entrenar XGBoost para obtener probabilidades
empresas_unicas <- unique(groups)
n_folds <- 5
folds <- createFolds(empresas_unicas, k = n_folds, list = TRUE)

all_true <- c()
all_proba <- c()

for (i in 1:n_folds) {
  test_empresas <- empresas_unicas[folds[[i]]]
  train_idx <- !(groups %in% test_empresas)
  test_idx <- groups %in% test_empresas
  
  X_train <- X[train_idx, ]
  X_test <- X[test_idx, ]
  y_train <- y[train_idx]
  y_test <- y[test_idx]
  
  dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
  model <- xgb.train(params = list(objective = "binary:logistic", max_depth = 6),
                     data = dtrain, nrounds = 100, verbose = 0)
  
  dtest <- xgb.DMatrix(data = as.matrix(X_test))
  prob <- predict(model, dtest)
  
  all_true <- c(all_true, y_test)
  all_proba <- c(all_proba, prob)
}

# Curva ROC
roc_obj <- roc(all_true, all_proba)
auc_value <- auc(roc_obj)

# Graficar curva ROC
roc_df <- data.frame(
  fpr = 1 - roc_obj$specificities,
  tpr = roc_obj$sensitivities
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  labs(x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)",
       title = paste("ROC Curve - XGBoost (AUC =", round(auc_value, 3), ")")) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal()

ggsave("curva_roc.png", width = 8, height = 6, dpi = 300)

# Histograma del ISE
ggplot(data, aes(x = ISE)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black") +
  labs(x = "Índice de Sostenibilidad Empresarial (ISE)", 
       y = "Frecuencia",
       title = "Distribución del ISE") +
  theme_minimal()

ggsave("histograma_ise.png", width = 8, height = 6, dpi = 300)

cat("Figuras guardadas: curva_roc.png, histograma_ise.png, curva_calibracion.png")