# 09_metricas_bootstrap_ml.R
# Bootstrap para intervalos de confianza de métricas ML

library(boot)

# Función de bootstrap para accuracy y AUC
boot_ml_metrics <- function(data, indices, features, target, group_col) {
  d <- data[indices, ]
  X <- d[, features]
  y <- as.factor(d[, target])
  groups <- d[, group_col]
  
  # Entrenar XGBoost con GroupKFold (simplificado)
  empresas_unicas <- unique(groups)
  n_folds <- 5
  folds <- createFolds(empresas_unicas, k = n_folds, list = TRUE)
  
  all_pred <- c()
  all_true <- c()
  
  for (i in 1:n_folds) {
    test_empresas <- empresas_unicas[folds[[i]]]
    train_idx <- !(groups %in% test_empresas)
    test_idx <- groups %in% test_empresas
    
    X_train <- X[train_idx, ]
    X_test <- X[test_idx, ]
    y_train <- y[train_idx]
    y_test <- y[test_idx]
    
    dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = as.numeric(y_train) - 1)
    model <- xgb.train(params = list(objective = "binary:logistic", max_depth = 6),
                       data = dtrain, nrounds = 100, verbose = 0)
    
    dtest <- xgb.DMatrix(data = as.matrix(X_test))
    prob <- predict(model, dtest)
    y_pred <- ifelse(prob >= 0.5, 1, 0)
    
    all_pred <- c(all_pred, y_pred)
    all_true <- c(all_true, as.numeric(y_test) - 1)
  }
  
  # Calcular accuracy
  accuracy <- mean(all_pred == all_true)
  
  # Calcular AUC (simplificado)
  library(pROC)
  roc_obj <- roc(all_true, all_pred, quiet = TRUE)
  auc <- as.numeric(roc_obj$auc)
  
  return(c(accuracy, auc))
}

# Ejecutar bootstrap
set.seed(42)
boot_results_ml <- boot(data, boot_ml_metrics, R = 500,
                        features = c("GS", "QCa", "Acc", "Y", "ROA", "Mo"),
                        target = "WCa", group_col = "empresa_id")

# Intervalos de confianza
cat("\n=== INTERVALOS DE CONFIANZA MÉTRICAS ML ===\n")
cat("Accuracy:\n")
print(boot.ci(boot_results_ml, index = 1, type = "perc"))
cat("\nAUC:\n")
print(boot.ci(boot_results_ml, index = 2, type = "perc"))