# 08_ml_groupkfold.R
# Machine Learning con validación cruzada por empresa (GroupKFold)

library(caret)
library(randomForest)
library(xgboost)

# Preparar datos para ML
# Variables predictoras
features <- c("GS", "QCa", "Acc", "Y", "ROA", "Mo")
X <- data[, features]
y <- as.factor(data$WCa)  # Variable dependiente (0/1)
groups <- data$empresa_id  # Agrupación por empresa

# Configurar GroupKFold manualmente
empresas_unicas <- unique(groups)
n_folds <- 5
folds <- createFolds(empresas_unicas, k = n_folds, list = TRUE)

# Almacenar resultados
resultados <- list()

# Definir modelos
modelos <- list(
  "Logistic Regression" = list(
    train = function(X_train, y_train) {
      glm(y_train ~ ., data = cbind(X_train, y_train), family = binomial)
    },
    predict = function(model, X_test) {
      prob <- predict(model, newdata = X_test, type = "response")
      ifelse(prob >= 0.5, 1, 0)
    }
  ),
  "Random Forest" = list(
    train = function(X_train, y_train) {
      randomForest(X_train, as.factor(y_train), ntree = 100, max_depth = 10)
    },
    predict = function(model, X_test) {
      as.numeric(predict(model, X_test)) - 1
    }
  ),
  "XGBoost" = list(
    train = function(X_train, y_train) {
      dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
      xgb.train(params = list(
        objective = "binary:logistic",
        learning_rate = 0.1,
        max_depth = 6,
        subsample = 0.8,
        colsample_bytree = 0.8,
        eval_metric = "loglik"
      ), data = dtrain, nrounds = 100, verbose = 0)
    },
    predict = function(model, X_test) {
      dtest <- xgb.DMatrix(data = as.matrix(X_test))
      prob <- predict(model, dtest)
      ifelse(prob >= 0.5, 1, 0)
    }
  )
)

# Validación cruzada
for (model_name in names(modelos)) {
  cat("\n=== ", model_name, " ===\n")
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
    
    model <- modelos[[model_name]]$train(X_train, y_train)
    y_pred <- modelos[[model_name]]$predict(model, X_test)
    
    all_pred <- c(all_pred, y_pred)
    all_true <- c(all_true, y_test)
  }
  
  # Calcular métricas
  cm <- confusionMatrix(as.factor(all_pred), as.factor(all_true))
  resultados[[model_name]] <- cm$overall
  print(cm$overall)
}

cat("\n=== RESULTADOS COMPARATIVOS ===\n")
print(resultados)