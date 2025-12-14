dd=df_all_transformed
dd=dd[,-1]

dd_train <- dd[dd$dataset == "train", ]
dd_train=dd_train[,-2]
dd_test=dd[dd$dataset == "test", ]
dd_test=dd_test[,-2]

library(e1071)
library(mlbench)
library(ggplot2)
library(ISLR)

set.seed(9909)

svm_cv <- tune(
  "svm",
  song_popularity ~ ., 
  data = dd_train,
  kernel = "radial",
  ranges = list(
    cost = c(10),
    gamma = c(0.8)
  ),
  tunecontrol = tune.control(sampling = "cross", cross = 5)
)

summary(svm_cv)

# Millor model
best_svm <- svm_cv$best.model
best_svm

sqrt(svm_cv$best.performance)

dd_test2 <- dd_test
dd_test2$song_popularity <- NULL

preds <- predict(best_svm, dd_test2)

dd_test$pred_song_popularity <- preds

r=data.frame(id=1:5649,song_popularity=round(preds,0))

r$song_popularity
c=data.frame(id=1:5649,song_popularity=r$song_popularity)

write.csv(c,"SVM_final.csv")

# --------- 

set.seed(123)

train_index <- createDataPartition(dd_train$song_popularity, p = 0.7, list = FALSE)
train_data <- dd_train[train_index, ]
test_data <- dd_train[-train_index, ]


svm_cv <- tune(
  "svm",
  song_popularity ~ ., 
  data = train_data,
  kernel = "radial",
  ranges = list(
    cost = c(10),
    gamma = c(0.8)
  ),
  tunecontrol = tune.control(sampling = "cross", cross = 5)
)

summary(svm_cv)

# Millor model segons la cross-validation
best_svm <- svm_cv$best.model

# Prediccions sobre test_data
pred_test <- predict(best_svm, newdata = test_data)

vec <- as.vector(pred_test)
test_data$song_popularity-vec

rmse <- (test_data$song_popularity - vec)^2

n <- length(vec)
mape_obs <- numeric(n)


for (i in 1:n) {
  if (test_data$song_popularity[i] == 0) {
    true_val <- test_data$song_popularity[i] + 1
    pred_val <- vec[i] + 1
  } else {
    true_val <- test_data$song_popularity[i]
    pred_val <- vec[i]
  }
  
  mape_obs[i] <- abs((true_val - pred_val) / true_val)
}

mape=mape_obs

RSME=sqrt(mean(rmse))
MAPE=mean(mape)
SD_RSME=sd(sqrt(rmse))
SD_MAPE=sd(mape)

RSME
MAPE
SD_RSME
SD_MAPE
