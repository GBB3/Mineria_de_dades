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

best_oof_rmse <- Inf
best_oof_rf   <- rep(NA_real_, length(y_train))
best_params   <- NULL

for(i in seq_len(nrow(grid))){
  params <- grid[i, ]
  
  fold_rmse <- numeric(length(folds))
  fold_mape <- numeric(length(folds))
  oof_tmp <- rep(NA_real_, length(y_train))
  
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
    
    oof_tmp[idx_val] <- pred_val
    fold_rmse[f] <- rmse_vec(y_train[idx_val], pred_val)
    fold_mape[f] <- mape_metric(y_train[idx_val], pred_val)
  }
  
  grid$cv_rmse[i] <- mean(fold_rmse)
  grid$cv_mape[i] <- mean(fold_mape)
  
  if(grid$cv_rmse[i] < best_oof_rmse){
    best_oof_rmse <- grid$cv_rmse[i]
    best_oof_rf   <- oof_tmp
    best_params   <- params
  }
  
  if(i %% 10 == 0) cat("Tuning progress:", i, "/", nrow(grid), "\n")
}

grid <- grid[order(grid$cv_rmse), ]
print(head(grid, 10))

best <- grid[1, ]
cat("\nBEST PARAMS (by CV RMSE):\n")
print(best)

saveRDS(best_oof_rf, "oof_rf_best.rds")
saveRDS(best_params,  "best_params_rf_from_oof.rds")

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
pred_rf <- pred_test
# ============================================================
# SUBMISSION
# ============================================================

prediccions <- data.frame(
  id = 1:5649,
  song_popularity = pred_test
)

write.csv(prediccions, "RandFor2.csv", row.names = FALSE)


#-------------------------------------------------------------------------


# -----------------------
#  Llibreries
# -----------------------
required_pkgs <- c("data.table","dplyr","ggplot2","randomForest","glmnet",
                   "xgboost","caret","Matrix","corrplot","car","recipes","yardstick")
install_if_missing <- function(pkgs){
  to_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if(length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
}
install_if_missing(required_pkgs)

library(data.table)
library(dplyr)
library(ggplot2)
library(randomForest)
library(glmnet)
library(xgboost)
library(caret)
library(Matrix)
library(corrplot)
library(car)
library(recipes)
library(yardstick)

# Si alguna llibreria no es carrega, no abortis; avisa per pantalla
for(p in required_pkgs){
  if(!suppressWarnings(require(p, character.only = TRUE))){
    warning(paste("No s'ha pogut carregar", p))
  }
}

# -----------------------
# Funcions utilitats
# -----------------------
rmse_metric <- function(truth, pred){
  truth <- as.numeric(truth); pred <- as.numeric(pred)
  return(sqrt(mean((truth - pred)^2, na.rm = TRUE)))
}

#
mape_metric <- function(truth, pred){
  truth <- as.numeric(truth)
  pred  <- as.numeric(pred)
  idx0 <- which(truth == 0)
  out <- abs(truth - pred) / abs(truth)
  if(length(idx0) > 0){
    out[idx0] <- (abs(truth[idx0] - pred[idx0]) + 1) / (abs(truth[idx0]) + 1)
  }
  return(mean(out, na.rm = TRUE))
}


cv_oof_custom <- function(X, y, folds, fit_predict_fn){
  oof <- rep(NA_real_, length(y))
  rmse_vals <- numeric(length(folds))
  mape_vals <- numeric(length(folds))
  
  for(i in seq_along(folds)){
    idx_val <- folds[[i]]
    idx_tr  <- setdiff(seq_len(nrow(X)), idx_val)
    
    X_tr <- X[idx_tr, , drop=FALSE]; y_tr <- y[idx_tr]
    X_val <- X[idx_val, , drop=FALSE]; y_val <- y[idx_val]
    
    preds_val <- fit_predict_fn(X_tr, y_tr, X_val)
    
    oof[idx_val] <- preds_val
    rmse_vals[i] <- rmse_metric(y_val, preds_val)
    mape_vals[i] <- mape_metric(y_val, preds_val)
  }
  
  list(
    oof = oof,
    mean_rmse = mean(rmse_vals),
    sd_rmse   = sd(rmse_vals),
    mean_mape = mean(mape_vals),
    sd_mape   = sd(mape_vals)
  )
}


# -----------------------
# Llegir dades
# -----------------------

df <- df_all_transformed

TARGET <- "song_popularity"
if(!(TARGET %in% names(df))) stop(paste("No trobo la columna objectiu", TARGET))

# -----------------------
# Separar train/test
# -----------------------
df$global_ID<-NULL
df$ID<-NULL
train <- df[df$dataset=="train", ]
test  <- df[df$dataset=="test", ]
train$dataset<-NULL
test$dataset<-NULL
df$dataset<-NULL

cat("Observacions totals:", nrow(df), "  Train:", nrow(train), "  Test:", nrow(test), "\n")

# Creem recipe per preprocessing
rec <- recipe(as.formula(paste(TARGET, "~ .")), data = train) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

rec_prep <- prep(rec, training = train, verbose = FALSE)
train_processed <- bake(rec_prep, new_data = train)
test_processed  <- bake(rec_prep, new_data = test)

cat("Dimensions després preprocess:", dim(train_processed), dim(test_processed), "\n")

# -----------------------
#Multicolinearitat (VIF)
# -----------------------
numeric_for_vif <- train_processed %>%
  select(-one_of(TARGET)) %>%
  select_if(is.numeric)

if (ncol(numeric_for_vif) > 1) {
  nzv <- nearZeroVar(numeric_for_vif)
  if (length(nzv) > 0)
    numeric_for_vif <- numeric_for_vif[, -nzv]
  
  df_vif <- cbind(song_popularity = train_processed[[TARGET]],
                  numeric_for_vif)
  
  lm_tmp <- lm(song_popularity ~ ., data = df_vif)
  
  try({
    vif_vals <- vif(lm_tmp)
    vif_sorted <- sort(vif_vals, decreasing = TRUE)
    cat("\nTop VIF:\n")
    print(head(vif_sorted, 20))
  }, silent = TRUE)
}

# ----------------------
# Feature selection  amb RandomForest
# -----------------------
set.seed(42)
rf_for_sel <- randomForest(x = train_processed %>% select(-one_of(TARGET)),
                           y = train_processed[[TARGET]],
                           ntree = 300, importance = TRUE)
imp <- importance(rf_for_sel, type = 1)
imp_df <- data.frame(variable = rownames(imp), importance = imp[,1], row.names = NULL)
imp_df <- imp_df[order(-imp_df$importance), ]
cat("Top 30 variables per importància RF:\n")
print(head(imp_df, 30))

imp_df$cum <- cumsum(imp_df$importance)/sum(imp_df$importance)
selected_vars <- imp_df %>% filter(cum <= 0.98) %>% pull(variable)
if(length(selected_vars) < 20) selected_vars <- head(imp_df$variable, 100)
cat("Nombre variables seleccionades:", length(selected_vars), "\n")

X_all <- train_processed %>% select(all_of(selected_vars))
X_test  <- test_processed  %>% select(all_of(selected_vars))
y_all <- train_processed[[TARGET]]

# -----------------------
# CV - 5
# -----------------------
set.seed(123)
n_folds <- 5
folds <- createFolds(y_all, k = n_folds, list = TRUE, returnTrain = FALSE)

# -----------------------
# Model: XGBoost
# -----------------------
fit_predict_xgb <- function(X_tr, y_tr, X_val, params){
  dtrain <- xgb.DMatrix(data = as.matrix(X_tr), label = y_tr)
  dval   <- xgb.DMatrix(data = as.matrix(X_val))
  watchlist <- list(train = dtrain)
  set.seed(42)
  bst <- xgb.train(params = params, data = dtrain, nrounds = params$nrounds, verbose = 0)
  preds <- predict(bst, dval)
  return(as.numeric(preds))
}

xgb_grid <- expand.grid(eta = c(0.01, 0.05, 0.1),
                        max_depth = c(3,5,8),
                        subsample = c(0.7, 1.0),
                        colsample = c(0.5, 0.8),
                        nrounds = c(200, 400))



xgb_results <- data.frame(idx = seq_len(nrow(xgb_grid)), eta = xgb_grid$eta,
                          max_depth = xgb_grid$max_depth, subsample = xgb_grid$subsample,
                          colsample = xgb_grid$colsample, nrounds = xgb_grid$nrounds,
                          mean_rmse = NA, sd_rmse = NA,
                          mean_mape = NA, sd_mape = NA)

best_xgb_rmse <- Inf
best_oof_xgb  <- rep(NA_real_, length(y_all))
best_xgb_params <- NULL
best_xgb_nrounds <- NULL

for(i in seq_len(nrow(xgb_grid))){
  params <- list(objective = "reg:squarederror",
                 eta = xgb_grid$eta[i],
                 max_depth = xgb_grid$max_depth[i],
                 subsample = xgb_grid$subsample[i],
                 colsample_bytree = xgb_grid$colsample[i],
                 nrounds = xgb_grid$nrounds[i])
  
  res <- cv_oof_custom(X_all, y_all, folds,
                       function(Xtr,ytr,Xval) fit_predict_xgb(Xtr,ytr,Xval,params))
  
  xgb_results$mean_rmse[i] <- res$mean_rmse
  xgb_results$sd_rmse[i]   <- res$sd_rmse
  xgb_results$mean_mape[i] <- res$mean_mape
  xgb_results$sd_mape[i]   <- res$sd_mape
  
  if(res$mean_rmse < best_xgb_rmse){
    best_xgb_rmse    <- res$mean_rmse
    best_oof_xgb     <- res$oof
    best_xgb_params  <- params
    best_xgb_nrounds <- params$nrounds
  }
  
  cat(sprintf("XGB idx=%d (eta=%.3f,depth=%d,nrounds=%d) -> RMSE %.4f | MAPE %.4f\n",
              i, params$eta, params$max_depth, params$nrounds,
              res$mean_rmse, res$mean_mape))
}

summary_table <- data.frame(
  model = paste0("xgb_idx_", xgb_results$idx),
  mean_rmse = xgb_results$mean_rmse,
  sd_rmse   = xgb_results$sd_rmse,
  mean_mape = xgb_results$mean_mape,
  sd_mape   = xgb_results$sd_mape
)

summary_table <- summary_table[order(summary_table$mean_rmse), ]
cat("\n--- Resum comparatiu (ordre per RMSE) ---\n")
print(head(summary_table, 20))

best_row <- summary_table[1, ]
cat("\nMillor opció segons CV RMSE:", best_row$model, "amb RMSE", best_row$mean_rmse, "\n")

stopifnot(!any(is.na(best_oof_xgb)))
cat("\nSaved best XGB OOF RMSE (check):", rmse_metric(y_all, best_oof_xgb), "\n")

saveRDS(best_oof_xgb, "oof_xgb_best.rds")
saveRDS(best_xgb_params, "best_xgb_params_with_oof.rds")
saveRDS(best_xgb_nrounds, "best_xgb_nrounds_with_oof.rds")

# -----------------------
# Entrenar millor model
# -----------------------
best_model_name <- as.character(best_row$model)

if(grepl("^xgb", best_model_name)){
  
  idx <- as.numeric(sub("xgb_idx_", "", best_model_name))
  params <- list(
    objective = "reg:squarederror",
    eta = xgb_grid$eta[idx],
    max_depth = xgb_grid$max_depth[idx],
    subsample = xgb_grid$subsample[idx],
    colsample_bytree = xgb_grid$colsample[idx]
  )
  
  nrounds_best <- xgb_grid$nrounds[idx]
  
  cat("Entrenant XGBoost final amb paràmetres:\n")
  print(params)
  cat("nrounds =", nrounds_best, "\n")
  
  dtrain_full <- xgb.DMatrix(data = as.matrix(X_all), label = y_all)
  
  final_mod <- xgb.train(
    params = params,
    data = dtrain_full,
    nrounds = nrounds_best,
    verbose = 0
  )
  
  predict_final <- function(X_new){
    predict(final_mod, xgb.DMatrix(as.matrix(X_new)))
  }
  
} else {
  stop("Error detectant el millor model.")
}

# -----------------------
# Prediccions sobre TEST
# -----------------------
predictions <- predict_final(X_test)
pred_xgb <- predictions
prediccions<-data.frame(id=1:5649,song_popularity=predictions)
write.csv(prediccions, "rfsenseoutInstruKeyAudioSenseAUDIOVALENCE.csv", row.names = FALSE)

#--------------------------------------------------------------------------

# =========================
# CARICA OOF (validation predictions)
# =========================
# Se li hai già in memoria come best_oof_rf / best_oof_xgb, puoi saltare readRDS
oof_rf  <- readRDS("oof_rf_best.rds")   # OOF del miglior Ranger
oof_xgb <- readRDS("oof_xgb_best.rds")  # OOF del miglior XGBoost

# ATTENZIONE: y deve essere lo stesso target usato per generare le OOF
# Nel tuo codice può chiamarsi y_train (RF) o y_all (XGB). Qui assumo y_train.
y <- y_train
# Se invece stai nel file XGB e lì hai y_all:
# y <- y_all

stopifnot(length(oof_rf) == length(y), length(oof_xgb) == length(y))
stopifnot(!any(is.na(oof_rf)), !any(is.na(oof_xgb)))

# =========================
# 1) RMSE validation dei due modelli
# =========================
rmse_rf  <- rmse_metric(y, oof_rf)
rmse_xgb <- rmse_metric(y, oof_xgb)

cat("\nValidation RMSE (OOF) - Ranger RF :", rmse_rf,  "\n")
cat("Validation RMSE (OOF) - XGBoost   :", rmse_xgb, "\n")

# =========================
# 2) TUNING PESO ENSEMBLE
#    pred_ens = w * xgb + (1-w) * rf
# =========================
ws <- seq(0, 1, by = 0.01)  # griglia pesi (puoi fare 0.02 se vuoi più veloce)

rmse_w <- sapply(ws, function(w){
  pred_ens <- w * oof_xgb + (1 - w) * oof_rf
  rmse_metric(y, pred_ens)
})
mape_w <- sapply(ws, function(w){
  pred_ens <- w * oof_xgb + (1 - w) * oof_rf
  mape_metric(y, pred_ens)
})
mape_ens <- mape_metric(y, w_best * oof_xgb + (1 - w_best) * oof_rf)

cat("Validation MAPE (OOF) - Ensemble:", mape_ens, "\n")


best_idx <- which.min(rmse_w)
w_best <- ws[best_idx]
rmse_ens <- rmse_w[best_idx]

cat("\nBest weight w (XGB):", w_best, "\n")
cat("Validation RMSE (OOF) - Ensemble:", rmse_ens, "\n")

# (opzionale) mostra top 10 pesi
top10 <- data.frame(w = ws, rmse = rmse_w)
top10 <- top10[order(top10$rmse), ]
cat("\nTop 10 weights by RMSE:\n")
print(head(top10, 10))

# submission
pred_ensemble <- w_best * pred_xgb + (1 - w_best) * pred_rf
submission <- data.frame(
  id = 1:length(pred_ensemble),
  song_popularity = pred_ensemble
)

write.csv(submission, "ensxgrf.csv", row.names = FALSE)


