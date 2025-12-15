# ============================================================
# SETUP
# ============================================================

library(ranger)
library(caret)

set.seed(123)

mape_metric <- function(truth, pred){
  truth <- as.numeric(truth); pred <- as.numeric(pred)
  idx0 <- which(truth == 0)
  out <- abs(truth - pred) / abs(truth)
  if(length(idx0) > 0){
    out[idx0] <- (abs(truth[idx0] - pred[idx0]) + 1) / (abs(truth[idx0]) + 1)
  }
  mean(out, na.rm = TRUE)
}

df <- df_all_transformed
TARGET <- "song_popularity"
if(!(TARGET %in% names(df))) stop(paste("No trobo la columna objectiu", TARGET))

# pulizia ID
df$global_ID <- NULL
df$ID <- NULL

train_df <- df[df$dataset == "train", ]
test_df  <- df[df$dataset == "test",  ]
train_df$dataset <- NULL
test_df$dataset  <- NULL

cat("Observacions totals:", nrow(df), " Train:", nrow(train_df), " Test:", nrow(test_df), "\n")

y_train <- train_df[[TARGET]]
X_train <- train_df; X_train[[TARGET]] <- NULL
X_test  <- test_df;  X_test[[TARGET]]  <- NULL

stopifnot(nrow(X_test) > 0, nrow(X_train) > 0)

# ============================================================
# CV SETUP (5-fold)
# ============================================================

folds <- createFolds(y_train, k = 5, returnTrain = FALSE)

rmse_vec <- function(y_true, y_pred) sqrt(mean((y_true - y_pred)^2, na.rm = TRUE))

# ============================================================
# GRID DI PARAMETRI (sensata, non enorme)
# ============================================================

p <- ncol(X_train)

grid <- expand.grid(
  num.trees       = c(800, 1500),
  mtry            = unique(pmax(1, c(floor(sqrt(p)), floor(p/3), floor(p/2)))),
  min.node.size   = c(1, 5, 10, 20),
  sample.fraction = c(0.6, 0.8, 1.0),
  splitrule       = c("variance", "extratrees")
)

# se la griglia ti viene troppo grande, taglia qui:
# grid <- grid[1:80, ]

grid$cv_rmse <- NA_real_
grid$cv_mape <- NA_real_

# ============================================================
# TUNING CON CV
# ============================================================

for(i in seq_len(nrow(grid))){
  params <- grid[i, ]
  
  fold_rmse <- numeric(length(folds))
  fold_mape <- numeric(length(folds))
  
  for(f in seq_along(folds)){
    idx_val <- folds[[f]]
    idx_tr  <- setdiff(seq_along(y_train), idx_val)
    
    model <- ranger(
      x = X_train[idx_tr, , drop = FALSE],
      y = y_train[idx_tr],
      num.trees       = params$num.trees,
      mtry            = params$mtry,
      min.node.size   = params$min.node.size,
      sample.fraction = params$sample.fraction,
      splitrule       = as.character(params$splitrule),
      respect.unordered.factors = "order",
      seed = 123,
      importance = "none"
    )
    
    pred_val <- predict(model, data = X_train[idx_val, , drop = FALSE])$predictions
    
    fold_rmse[f] <- rmse_vec(y_train[idx_val], pred_val)
    fold_mape[f] <- mape_metric(y_train[idx_val], pred_val)
  }
  
  grid$cv_rmse[i] <- mean(fold_rmse)
  grid$cv_mape[i] <- mean(fold_mape)
  
  if(i %% 10 == 0) cat("Tuning progress:", i, "/", nrow(grid), "\n")
}

grid <- grid[order(grid$cv_rmse), ]
print(head(grid, 10))

best <- grid[1, ]
cat("\nBEST PARAMS (by CV RMSE):\n")
print(best)

# ============================================================
# TRAIN FINALE (su tutto il train) + METRICHE OOB DI CONTROLLO
# ============================================================

rf_final <- ranger(
  x = X_train,
  y = y_train,
  num.trees       = best$num.trees,
  mtry            = best$mtry,
  min.node.size   = best$min.node.size,
  sample.fraction = best$sample.fraction,
  splitrule       = as.character(best$splitrule),
  respect.unordered.factors = "order",
  seed = 123,
  importance = "impurity"
)

cat("\nOOB RMSE (control):", sqrt(rf_final$prediction.error), "\n")
cat("OOB MAPE (control):", mape_metric(y_train, rf_final$predictions), "\n")

# ============================================================
# ENSEMBLE SU PIÙ SEED (MEDIA DELLE PREDIZIONI TEST)
# ============================================================

seeds_ens <- c(11, 22, 33, 44, 55)  # puoi aumentare a 10 se vuoi
pred_mat <- matrix(NA_real_, nrow = nrow(X_test), ncol = length(seeds_ens))

for(s in seq_along(seeds_ens)){
  sd <- seeds_ens[s]
  
  m <- ranger(
    x = X_train,
    y = y_train,
    num.trees       = best$num.trees,
    mtry            = best$mtry,
    min.node.size   = best$min.node.size,
    sample.fraction = best$sample.fraction,
    splitrule       = as.character(best$splitrule),
    respect.unordered.factors = "order",
    seed = sd,
    importance = "none"
  )
  
  pred_mat[, s] <- predict(m, data = X_test)$predictions
  cat("Ensemble seed done:", sd, "\n")
}

pred_test <- rowMeans(pred_mat)

# ============================================================
# SUBMISSION
# ============================================================

prediccions <- data.frame(
  id = 1:5649,
  song_popularity = pred_test
)

write.csv(prediccions, "RandFor2.csv", row.names = FALSE)
