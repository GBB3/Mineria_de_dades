mape_metric <- function(truth, pred){
  truth <- as.numeric(truth)
  pred  <- as.numeric(pred)
  idx0 <- which(truth == 0)
  out <- abs(truth - pred) / abs(truth)
  if(length(idx0) > 0){
    out[idx0] <- (abs(truth[idx0] - pred[idx0]) + 1) /
      (abs(truth[idx0]) + 1)
  }
  mean(out, na.rm = TRUE)
}

df <- df_all_transformed

TARGET <- "song_popularity"
if(!(TARGET %in% names(df))) stop(paste("No trobo la columna objectiu", TARGET))

df$global_ID<-NULL
df$ID<-NULL
train <- df[df$dataset=="train", ]
test  <- df[df$dataset=="test", ]
train$dataset<-NULL
test$dataset<-NULL
df$dataset<-NULL

cat("Observacions totals:", nrow(df), "  Train:", nrow(train), "  Test:", nrow(test), "\n")

# ============================================================
# SEPARACIÓN DE VARIABLES (X) Y TARGET (y)
# ============================================================

y_train <- train[[TARGET]]
X_train <- train
X_train[[TARGET]] <- NULL

# En el conjunto test NO tenemos el target
X_test <- test
X_test[[TARGET]] <- NULL

# ============================================================
# RANDOM FOREST CON RANGER (TUNING CON ERROR OOB)
# ============================================================

library(ranger)

set.seed(123)

param_grid <- expand.grid(
  num_trees = c(200, 500, 1000),
  mtry      = c(5, 10, floor(sqrt(ncol(X_train))), ncol(X_train)),
  max_depth = c(5, 10, 20)
)

oob_error <- rep(NA, nrow(param_grid))

for(i in 1:nrow(param_grid)){
  
  modelo <- ranger(
    x = X_train,
    y = y_train,
    num.trees = param_grid$num_trees[i],
    mtry      = param_grid$mtry[i],
    max.depth = param_grid$max_depth[i],
    seed      = 123
  )
  
  # ranger devuelve MSE OOB → lo convertimos a RMSE
  oob_error[i] <- sqrt(modelo$prediction.error)
}

# Resultados OOB
resultados_ranger <- param_grid
resultados_ranger$oob_rmse <- oob_error
resultados_ranger <- resultados_ranger[order(resultados_ranger$oob_rmse), ]

print(head(resultados_ranger))


# ============================================================
# MODELO FINAL RANDOM FOREST (RANGER)
# ============================================================

best_ranger <- resultados_ranger[1, ]

set.seed(123)
rf_ranger_final <- ranger(
  x = X_train,
  y = y_train,
  num.trees = best_ranger$num_trees,
  mtry      = best_ranger$mtry,
  max.depth = best_ranger$max_depth,
  importance = "impurity",
  seed = 123
)

print(rf_ranger_final)


# ============================================================
# MÉTRICAS INTERNAS - RANGER
# ============================================================
#RMSE
rmse_oob_ranger <- sqrt(rf_ranger_final$prediction.error)

cat("RMSE de validación OOB (Ranger):", rmse_oob_ranger, "\n")

# MAPE 
mape_oob_ranger <- mape_metric(y_train,rf_ranger_final$predictions)

cat("MAPE de validación OOB (Ranger):", mape_oob_ranger, "\n")

# ============================================================
# RANDOM FOREST CON CARET + CROSS-VALIDATION
# ============================================================

library(caret)

set.seed(123)

train_ctrl <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

param_grid_caret <- expand.grid(
  mtry = c(5, 10, floor(sqrt(ncol(X_train))), ncol(X_train))
)

rf_caret_cv <- train(
  x = X_train,
  y = y_train,
  method = "rf",
  metric = "RMSE",
  trControl = train_ctrl,
  tuneGrid = param_grid_caret,
  ntree = 1000,
  importance = TRUE
)

print(rf_caret_cv)
plot(rf_caret_cv)

# ============================================================
# MODELO FINAL RANDOM FOREST (CARE T)
# ============================================================

best_mtry <- rf_caret_cv$bestTune$mtry
library(randomForest)

set.seed(123)
rf_caret_final <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 1000,
  mtry = best_mtry,
  importance = TRUE
)

# ============================================================
# MÉTRICAS INTERNAS - CARET
# ============================================================

# RMSE de validación (CV)
rmse_cv_caret <- min(rf_caret_cv$results$RMSE)
cat("RMSE de validación CV (Caret):", rmse_cv_caret, "\n")

# MAPE de validación (CV)
preds_cv <- rf_caret_cv$pred

mape_cv_caret <- mape_metric(preds_cv$obs,preds_cv$pred)

cat("MAPE de validación CV (Caret):", mape_cv_caret, "\n")

print(rf_caret_final)

