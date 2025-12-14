library(MASS)
library(ISLR)
library(vcd)
library(caret)
str(df_outliers)
##Partición de Data usando library(caret)

# KPI
KPI <- function(data, lev = NULL, model = NULL) {
  eps <- 1e-6  
  mape <- mean(abs((data$obs - data$pred) / (data$obs + eps))) * 100
  
  c(
    RMSE = RMSE(data$pred, data$obs),
    Rsquared = R2(data$pred, data$obs),
    MAPE = mape
  )
}

#------
set.seed(1)
trainingg <- subset(df_outliers, dataset == "train")
inTraining <- createDataPartition(trainingg$song_popularity, p = .80, list = FALSE)
training <- trainingg[inTraining,]
testing  <- trainingg[-inTraining,]
training <- training[, -c(1:3, ncol(training))]
testing <- testing[, -c(1:3, ncol(testing))]
set.seed(1)

model3 <- train(
  song_popularity ~ .,
  data = training,
  method = 'knn',
  preProcess = c("center", "scale"),
  tuneGrid = data.frame(k = c(3, 5, 7))
)
model3
###
test.features = subset(testing, select=-c(song_popularity))
test.target = subset(testing, select=song_popularity)[,1]

predictions = predict(model3, newdata = test.features)
# RMSE
sqrt(mean((test.target - predictions)^2))
# R2
cor(test.target, predictions) ^ 2
# MAPE
eps <- 1e-6 
mean(abs((test.target - predictions) / (test.target + eps))) * 100
### Si agregamos cross-validation
ctrl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = KPI)

model4 <- train(
  song_popularity ~ .,
  data = training,
  method = 'knn',
  preProcess = c("center", "scale"),
  trControl = ctrl
)
model4
test.features = subset(testing, select=-c(song_popularity))
test.target = subset(testing, select=song_popularity)[,1]
predictions = predict(model4, newdata = test.features)

# RMSE
sqrt(mean((test.target - predictions)^2))
# R2
cor(test.target, predictions) ^ 2
# MAPE
eps <- 1e-6 
mean(abs((test.target - predictions) / (test.target + eps))) * 100

##Tunning the model (GridSearch for k)
set.seed(1)

tuneGrid <- expand.grid(
  k = seq(5, 9, by = 1)
)

model5 <- train(
  song_popularity ~ .,
  data = training,
  method = 'knn',
  preProcess = c("center", "scale"),
  trControl = ctrl,
  tuneGrid = tuneGrid
)
model5
plot(model5)
# RMSE
sqrt(mean((test.target - predictions)^2))
# R2
cor(test.target, predictions) ^ 2
# MAPE
eps <- 1e-6 
mean(abs((test.target - predictions) / (test.target + eps))) * 100

