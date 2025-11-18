# OPTUNA
library(reticulate)

# Import Python modules
optuna <- import("optuna")
np <- import("numpy")
pd <- import("pandas")
sk <- import("sklearn")
pt <- import("sklearn.preprocessing")
ensemble <- import("sklearn.ensemble")

# Carregar dataset R → Python
df <- as.data.frame(df)         # assumeixes que tens df a R
df_py <- r_to_py(df)

# Separar X i y
X <- df_py$drop("song_popularity", axis = 1L)
y <- df_py[["song_popularity"]]

transformations <- list(
  "none"   = NULL,
  "log"    = function(x) np$log1p(x),
  "sqrt"   = function(x) np$sqrt(x),
  "standard" = function(x) pt$StandardScaler()$fit_transform(x$reshape(-1L,1L)),
  "minmax"   = function(x) pt$MinMaxScaler()$fit_transform(x$reshape(-1L,1L))
)

objective <- function(trial) {
  
  X_trans <- X$copy()
  
  # aplicar transformacions
  for (col in colnames(df)[colnames(df) != "song_popularity"]) {
    t <- trial$suggest_categorical(
      paste0("trans_", col),
      names(transformations)
    )
    
    if (t != "none") {
      X_trans[[col]] <- transformations[[t]](X[[col]])
    }
  }
  
  # Train-test split
  train_test_split <- sk$model_selection$train_test_split
  split <- train_test_split(X_trans, y, test_size = 0.2, random_state = 42L)
  
  X_train <- split[[1]]
  X_test  <- split[[2]]
  y_train <- split[[3]]
  y_test  <- split[[4]]
  
  # Model
  model <- ensemble$RandomForestRegressor()
  model$fit(X_train, y_train)
  preds <- model$predict(X_test)
  
  mape <- sk$metrics$mean_absolute_percentage_error(y_test, preds)
  return(mape)
}

study <- optuna$create_study(direction = "minimize")
study$optimize(objective, n_trials = 200L)

print(study$best_params)
print(study$best_value)

