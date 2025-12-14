############################################################
# GLM models with lambda.1se
# - GLM (Gaussian)
# - Ridge Regression
# - Elastic Net
# Metrics: RMSE and MAPE (5-fold CV)
############################################################

# -----------------------
# Libraries
# -----------------------
required_pkgs <- c("dplyr","caret","glmnet","recipes","yardstick")
install_if_missing <- function(pkgs){
  to_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if(length(to_install)) install.packages(to_install, repos="https://cloud.r-project.org")
}
install_if_missing(required_pkgs)

library(dplyr)
library(caret)
library(glmnet)
library(recipes)
library(yardstick)

# -----------------------
# Metrics
# -----------------------
rmse_metric <- function(truth, pred){
  sqrt(mean((as.numeric(truth) - as.numeric(pred))^2, na.rm = TRUE))
}

mape_metric <- function(truth, pred){
  truth <- as.numeric(truth)
  pred  <- as.numeric(pred)
  idx0 <- which(truth == 0)
  out <- abs(truth - pred) / abs(truth)
  if(length(idx0) > 0){
    out[idx0] <- (abs(truth[idx0] - pred[idx0]) + 1) / (abs(truth[idx0]) + 1)
  }
  mean(out, na.rm = TRUE)
}

# -----------------------
# Cross-validation utility
# -----------------------
cv_rmse_custom <- function(X, y, folds, fit_predict_fn){
  rmse_vals <- numeric(length(folds))
  mape_vals <- numeric(length(folds))
  
  for(i in seq_along(folds)){
    idx_val <- folds[[i]]
    idx_tr  <- setdiff(seq_len(nrow(X)), idx_val)
    
    X_tr <- X[idx_tr,,drop=FALSE]
    y_tr <- y[idx_tr]
    X_val <- X[idx_val,,drop=FALSE]
    y_val <- y[idx_val]
    
    preds <- fit_predict_fn(X_tr, y_tr, X_val)
    
    rmse_vals[i] <- rmse_metric(y_val, preds)
    mape_vals[i] <- mape_metric(y_val, preds)
  }
  
  list(
    mean_rmse = mean(rmse_vals),
    sd_rmse   = sd(rmse_vals),
    mean_mape = mean(mape_vals),
    sd_mape   = sd(mape_vals)
  )
}

# -----------------------
# Load data
# -----------------------
load("df_all_transformedsensekeyaudioNOTEMPO.RData")
df <- df_all_transformed

TARGET <- "song_popularity"

df$global_ID <- NULL
df$ID <- NULL

train <- df[df$dataset == "train", ]
test  <- df[df$dataset == "test",  ]

train$dataset <- NULL
test$dataset  <- NULL
df$dataset    <- NULL

cat("Train size:", nrow(train),
    "| Test size:", nrow(test), "\n")

# -----------------------
# Preprocessing
# -----------------------
rec <- recipe(as.formula(paste(TARGET, "~ .")), data = train) %>%
  step_YeoJohnson(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

rec_prep <- prep(rec, training = train)

train_proc <- bake(rec_prep, new_data = train)
test_proc  <- bake(rec_prep, new_data = test)

X <- as.matrix(train_proc %>% select(-one_of(TARGET)))
y <- train_proc[[TARGET]]

X_test <- as.matrix(
  test_proc %>% select(colnames(X))
)

# -----------------------
# Cross-validation folds
# -----------------------
set.seed(123)
folds <- createFolds(y, k = 5, list = TRUE)

# -----------------------
# GLM baseline
# -----------------------
fit_predict_glm <- function(X_tr, y_tr, X_val){
  df_tr <- data.frame(y = y_tr, X_tr)
  model <- glm(y ~ ., data = df_tr, family = gaussian())
  preds <- predict(model, newdata = data.frame(X_val))
  as.numeric(preds)
}

res_glm <- cv_rmse_custom(X, y, folds, fit_predict_glm)

# -----------------------
# Ridge Regression (lambda.1se)
# -----------------------
fit_predict_ridge <- function(X_tr, y_tr, X_val){
  cv_model <- cv.glmnet(X_tr, y_tr, alpha = 0, family = "gaussian")
  preds <- predict(cv_model, newx = X_val, s = "lambda.1se")
  as.numeric(preds)
}

res_ridge <- cv_rmse_custom(X, y, folds, fit_predict_ridge)

# -----------------------
# Elastic Net (lambda.1se)
# -----------------------
alpha_grid <- c(0.2, 0.5, 0.8)

enet_summary <- data.frame(
  model = paste0("enet_alpha_", alpha_grid),
  alpha = alpha_grid,
  mean_rmse = NA,
  mean_mape = NA
)

for(i in seq_along(alpha_grid)){
  
  a <- alpha_grid[i]
  
  fit_predict_enet <- function(X_tr, y_tr, X_val){
    cv_model <- cv.glmnet(X_tr, y_tr, alpha = a, family = "gaussian")
    preds <- predict(cv_model, newx = X_val, s = "lambda.1se")
    as.numeric(preds)
  }
  
  res <- cv_rmse_custom(X, y, folds, fit_predict_enet)
  
  enet_summary$mean_rmse[i] <- res$mean_rmse
  enet_summary$mean_mape[i] <- res$mean_mape
  
  cat(sprintf("Elastic Net (lambda.1se) alpha=%.1f -> RMSE %.4f | MAPE %.4f\n",
              a, res$mean_rmse, res$mean_mape))
}

enet_summary <- enet_summary[order(enet_summary$mean_rmse), ]

# -----------------------
# Summary table
# -----------------------
results_table <- data.frame(
  model = c("GLM", "Ridge_1se", enet_summary$model[1]),
  mean_rmse = c(res_glm$mean_rmse,
                res_ridge$mean_rmse,
                enet_summary$mean_rmse[1]),
  mean_mape = c(res_glm$mean_mape,
                res_ridge$mean_mape,
                enet_summary$mean_mape[1])
)

print(results_table)

# -----------------------
# Final Elastic Net model (lambda.1se)
# -----------------------
best_alpha <- enet_summary$alpha[1]

final_model <- cv.glmnet(
  X, y,
  alpha = best_alpha,
  family = "gaussian"
)

test_predictions <- predict(
  final_model,
  newx = X_test,
  s = "lambda.1se"
)

submission <- data.frame(
  ID = 1:nrow(test),
  song_popularity = as.numeric(test_predictions)
)

write.csv(submission, "submission_glm_elastic_net_lambda1se.csv", row.names = FALSE)

cat("\nFinal model: Elastic Net (alpha =", best_alpha,
    ", lambda = lambda.1se )\n")
