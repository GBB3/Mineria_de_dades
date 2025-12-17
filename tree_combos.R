###http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/141-cart-model-decision-tree-essentials/
###Check Regression Trees above
library(MASS)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(ggpubr)
#install.packages("tree")
library(tree)

load("df_no_outliers_extrems.RData")
df_all_transformed <- df_no_outliers_extrems
datos_train <- subset(df_all_transformed, dataset == "train")
datos_test  <- subset(df_all_transformed, dataset == "test")
datos_train <- datos_train[, -c(1:3, ncol(datos_train))]
datos_test  <- datos_test[, -c(1:3, ncol(datos_test))]

set.seed(123)
arbol_regresion <- tree::tree(
  formula = song_popularity ~ .,
  data    = datos_train,
  split   = "deviance",
  mincut  = 20,
  minsize = 50,
)

summary(arbol_regresion)

# Pruning (const complexity pruning) with CV --> cross-validation
# ==============================================================================

# here the strategy by maximazing the size of the tree
ctrl <- tree.control(
  nobs = nrow(datos_train),
  mincut = 1,
  minsize = 2,
  mindev = 0.0001
)

arbol_regresion <- tree(
  song_popularity ~ .,
  data = datos_train,
  split = "deviance",
  control = ctrl
)

cv_arbol <- cv.tree(arbol_regresion, K = 5)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
paste("Optimal size:", size_optimo)
arbol_final <- prune.tree(
  tree = arbol_regresion,
  best = size_optimo)
predicciones <- as.numeric(predict(arbol_regresion, newdata = datos_test))

# Creem un data frame amb ID i prediccions
df_export <- data.frame(
  "ID" = seq_along(predicciones),
  song_popularity = predicciones
)

# Exportem a CSV
library(writexl)
write.csv(df_export, "tree_all_transformed.csv", row.names = FALSE, )
# ==============================================================================
# METODO ALGORITMO EXHAUSTIVO PARA LAS ENCONTRAR MEJOR TRANSFORMACIONES
# ==============================================================================
load("df_no_outliers_extrems.RData")
df_all_imputed <- df_no_outliers_extrems

datos_train <- subset(df_all_imputed, dataset == "train")
datos_test  <- subset(df_all_imputed, dataset == "test")
datos_train <- datos_train[, -c(1:3, ncol(datos_train))]
datos_test  <- datos_test[, -c(1:3, ncol(datos_test))]


set.seed(123)
train <- sample(1:nrow(datos_train), size = nrow(datos_train)/2)
train_train <- datos_train[train,]
train_test  <- datos_train[-train,]

set.seed(123)
arbol_regresion <- tree::tree(
  formula = song_popularity ~ .,
  data    = train_train,
  split   = "deviance",
  mincut  = 20,
  minsize = 50,
)

summary(arbol_regresion)

# Pruning (const complexity pruning) with CV --> cross-validation
# ==============================================================================

# here the strategy by maximazing the size of the tree
ctrl <- tree.control(
  nobs = nrow(train_train),
  mincut = 1,
  minsize = 2,
  mindev = 0.0001
)

arbol_regresion <- tree(
  song_popularity ~ .,
  data = train_train,
  split = "deviance",
  control = ctrl
)

cv_arbol <- cv.tree(arbol_regresion, K = 5)
size_optimo <- rev(cv_arbol$size)[which.min(rev(cv_arbol$dev))]
paste("Optimal size:", size_optimo)
arbol_final <- prune.tree(
  tree = arbol_regresion,
  best = size_optimo)
predicciones <- as.numeric(predict(arbol_regresion, newdata = train_test))
test_rmse    <- sqrt(mean((predicciones - train_test$song_popularity)^2))
paste("Root mean squared error (rmse) initial tree:", round(test_rmse,2))
predicciones <- predict(arbol_final, newdata = train_test)
test_rmse    <- sqrt(mean((predicciones - train_test$song_popularity)^2))
paste("rmse final tree:", round(test_rmse,2))

caret::postResample(predicciones, datos_test$medv)
##Another KPI to evaluate Accuracy in Regression --> MAPE
# Afegim un petit epsilon per evitar zero
epsilon <- 1e-8
error <- abs(predicciones - train_test$song_popularity) / (train_test$song_popularity + epsilon)
average <- mean(error) * 100
acc <- 100 - average
acc

### Complete Function for Accuracy KPIs
accuracy <- function(pred, obs, na.rm = FALSE, 
                     tol = sqrt(.Machine$double.eps)) {
  err <- obs - pred     # Errors
  if(na.rm) {
    is.a <- !is.na(err)
    err <- err[is.a]
    obs <- obs[is.a]
  }  
  perr <- 100*err/pmax(obs, tol)  # % errors
  return(c(
    me = mean(err),           # Mean error
    rmse = sqrt(mean(err^2)), # sqrt mean squared error
    mae = mean(abs(err)),     # mean absolute error
    mpe = mean(perr),         # mean percentage error
    mape = mean(abs(perr)),   # mean absolute percentage error
    r.squared = 1 - sum(err^2)/sum((obs - mean(obs))^2)
  ))
}
accuracy(predicciones, train_test$song_popularity)
# ==============================================================================
install.packages("bestNormalize")
library(bestNormalize)   # si ho tens instal·lat; util per YeoJohnson i trobar lambda
# install.packages("bestNormalize")

eps <- 1e-8

safe_identity <- function(x) x

safe_log1p <- function(x) {
  # log(1 + x) funciona amb x >= -1. Evitem -Inf
  out <- tryCatch(log1p(x), error = function(e) rep(NA_real_, length(x)))
  if (!is.numeric(out)) out <- as.numeric(out)
  out
}

safe_log <- function(x) {
  if (all(is.finite(x)) && all(x > 0, na.rm = TRUE)) return(log(x))
  return(rep(NA_real_, length(x)))
}

safe_log10 <- function(x) {
  if (all(is.finite(x)) && all(x > 0, na.rm = TRUE)) return(log10(x))
  return(rep(NA_real_, length(x)))
}

safe_sqrt <- function(x) {
  if (all(is.finite(x)) && all(x >= 0, na.rm = TRUE)) return(sqrt(x))
  return(rep(NA_real_, length(x)))
}

safe_reciprocal <- function(x) {
  # evita dividir per zero; afegim eps
  if (all(is.finite(x)) && all(abs(x) > 0, na.rm = TRUE)) return(1 / (x + eps))
  return(rep(NA_real_, length(x)))
}

safe_boxcox <- function(x) {
  # BoxCox requereix valors positius
  if (!all(is.finite(x))) return(rep(NA_real_, length(x)))
  if (any(x <= 0, na.rm = TRUE)) return(rep(NA_real_, length(x)))
  # troba lambda aprox amb bestNormalize::boxcox
  bt <- tryCatch(bestNormalize::boxcox(x, standardize = FALSE), error = function(e) NULL)
  if (is.null(bt)) return(rep(NA_real_, length(x)))
  return(predict(bt))
}

safe_yeojohnson <- function(x) {
  if (!all(is.finite(x))) return(rep(NA_real_, length(x)))
  bt <- tryCatch(bestNormalize::yeojohnson(x, standardize = FALSE), error = function(e) NULL)
  if (is.null(bt)) return(rep(NA_real_, length(x)))
  return(predict(bt))
}



safe_robust <- function(x) {
  med <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  if (iqr == 0) return(rep(NA_real_, length(x)))
  (x - med) / iqr
}

safe_rank <- function(x) {
  r <- ecdf(x)(x)        # percentil [0,1]
  as.numeric(r)
}

safe_winsor <- function(x, p = 0.01) {
  lo <- quantile(x, p, na.rm = TRUE)
  hi <- quantile(x, 1-p, na.rm = TRUE)
  pmax(pmin(x, hi), lo)
}

# ---- Llista de transformacions a provar ----
transformations <- list(
  identity = safe_identity,
  log1p    = safe_log1p,
  log      = safe_log
)

predictores <- setdiff(names(train_train), "song_popularity")
library(purrr)

# Noms de transformacions per cada variable
trans_names <- rep(list(names(transformations)), length(predictores))

# Totes les combinacions possibles
combos <- expand.grid(trans_names, stringsAsFactors = FALSE)
results <- list()

for(i in 1:nrow(combos)) {
  combo <- combos[i, ]
  
  train_mod <- train_train
  test_mod  <- train_test
  
  valid <- TRUE
  
  for(j in seq_along(predictores)) {
    var <- predictores[j]
    func <- transformations[[ combo[[j]] ]]
    
    new_train <- func(train_mod[[var]])
    new_test  <- func(test_mod[[var]])
    
    # Si la transformació dona NA (invalid), marquem com a no vàlid
    if (all(is.na(new_train))) {
      valid <- FALSE
      break
    }
    
    train_mod[[var]] <- new_train
    test_mod[[var]]  <- new_test
  }
  
  if (!valid) {
    next  # saltem aquesta combinació
  }
  
  ctrl <- tree.control(nobs = nrow(train_mod), mincut = 1, minsize = 2, mindev = 0.0001)
  tree_model <- tree(song_popularity ~ ., data = train_mod, split = "deviance", control = ctrl)

  
  # ---- PRUNING AMB CROSS-VALIDATION (ROBUST) ----
  
  # ---- PRUNING AMB CROSS-VALIDATION (A PROVA DE BUGS) ----
  
  tree_final <- tree_model
  size_opt <- 1
  
  cv_tree <- tryCatch(
    cv.tree(tree_model, K = 5),
    error = function(e) NULL
  )
  
  if (!is.null(cv_tree)) {
    
    size_opt_tmp <- rev(cv_tree$size)[which.min(rev(cv_tree$dev))]
    
    if (!is.na(size_opt_tmp) && size_opt_tmp > 1) {
      
      tree_try <- tryCatch(
        prune.tree(tree_model, best = size_opt_tmp),
        error = function(e) NULL
      )
      
      if (!is.null(tree_try)) {
        tree_final <- tree_try
        size_opt <- size_opt_tmp
      }
    }
  }
  
  # ---- PREDICCIÓ I RMSE ----
  predicciones <- predict(tree_final, newdata = test_mod)
  
  rmse <- sqrt(mean((predicciones - test_mod$song_popularity)^2))
  
  results[[i]] <- c(combo, rmse = rmse, size = size_opt)
  
  
  
  
  
}


    # Convertir resultats en data frame
results_df <- do.call(
  rbind,
  lapply(results, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE))
)

results_df$rmse <- as.numeric(results_df$rmse)
results_df$size <- as.numeric(results_df$size)

