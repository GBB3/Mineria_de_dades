# kaggle_pipeline_rmse.R
# Pipeline complet en R per minimitzar RMSE sobre el target `song_popularity`.
# Fitxer d'entrada (provat amb el teu upload): /mnt/data/df_all_imputed.csv
# Fitxer de sortida amb prediccions: /mnt/data/predictions_r.csv

# -----------------------
# 1) Llibreries
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
# 2) Funcions utilitats
# -----------------------
rmse_metric <- function(truth, pred){
  truth <- as.numeric(truth); pred <- as.numeric(pred)
  return(sqrt(mean((truth - pred)^2, na.rm = TRUE)))
}

# NOVA FUNCIÓ: MAPE AMB LA TEVA REGLA (si real = 0 → (|y-p|+1)/(1+1))
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

# CV manual que accepta funció d'entrenament/predicció
cv_rmse_custom <- function(X, y, folds, fit_predict_fn){
  rmse_vals <- numeric(length(folds))
  mape_vals <- numeric(length(folds))
  
  for(i in seq_along(folds)){
    idx_val <- folds[[i]]
    idx_tr  <- setdiff(seq_len(nrow(X)), idx_val)
    X_tr <- X[idx_tr,,drop=FALSE]; y_tr <- y[idx_tr]
    X_val <- X[idx_val,,drop=FALSE]; y_val <- y[idx_val]
    
    preds_val <- fit_predict_fn(X_tr, y_tr, X_val)
    
    rmse_vals[i] <- rmse_metric(y_val, preds_val)
    mape_vals[i] <- mape_metric(y_val, preds_val)
  }
  
  return(list(
    mean_rmse = mean(rmse_vals),
    sd_rmse   = sd(rmse_vals),
    fold_rmse = rmse_vals,
    mean_mape = mean(mape_vals),
    sd_mape   = sd(mape_vals),
    fold_mape = mape_vals
  ))
}

# -----------------------
# 3) Llegir dades
# -----------------------

df <- df_all_transformed

TARGET <- "song_popularity"
if(!(TARGET %in% names(df))) stop(paste("No trobo la columna objectiu", TARGET))

# -----------------------
# 4) Separar train/test
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
# 7) Multicolinearitat (VIF)
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

# -----------------------
# 8) Feature selection ràpida amb RandomForest
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
# 9) CV setup
# -----------------------
set.seed(123)
n_folds <- 5
folds <- createFolds(y_all, k = n_folds, list = TRUE, returnTrain = FALSE)

# -----------------------
# 12) Model 3: XGBoost
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

if(nrow(xgb_grid) > 12) set.seed(42); xgb_grid <- xgb_grid[sample(nrow(xgb_grid), 12), ]

xgb_results <- data.frame(idx = seq_len(nrow(xgb_grid)), eta = xgb_grid$eta,
                          max_depth = xgb_grid$max_depth, subsample = xgb_grid$subsample,
                          colsample = xgb_grid$colsample, nrounds = xgb_grid$nrounds,
                          mean_rmse = NA, sd_rmse = NA,
                          mean_mape = NA, sd_mape = NA)

for(i in seq_len(nrow(xgb_grid))){
  params <- list(objective = "reg:squarederror",
                 eta = xgb_grid$eta[i],
                 max_depth = xgb_grid$max_depth[i],
                 subsample = xgb_grid$subsample[i],
                 colsample_bytree = xgb_grid$colsample[i],
                 nrounds = xgb_grid$nrounds[i])
  
  res <- cv_rmse_custom(X_all, y_all, folds,
                        function(Xtr,ytr,Xval) fit_predict_xgb(Xtr,ytr,Xval,params))
  
  xgb_results$mean_rmse[i] <- res$mean_rmse
  xgb_results$sd_rmse[i]   <- res$sd_rmse
  xgb_results$mean_mape[i] <- res$mean_mape
  xgb_results$sd_mape[i]   <- res$sd_mape
  
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

prediccions<-data.frame(id=1:5649,song_popularity=predictions)
write.csv(prediccions, "rfsenseoutInstruKeyAudioSenseAUDIOVALENCE.csv", row.names = FALSE)
