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

set.seed(1234)

svm_cv <- tune(
  "svm",
  song_popularity ~ ., 
  data = dd_train,
  kernel = "radial",
  ranges = list(
    cost = c(10),
    gamma = c(0.8)
  )
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

r=data.frame(id=1:5649,song_popularity=round(preds_original,0))

r$song_popularity
c=data.frame(id=1:5649,song_popularity=r$song_popularity)

write.csv(c,"ere1.csv")


ggplot(
  data = svm_cv$performances,
  aes(x = cost, y = error, color = as.factor(gamma))
) +
  geom_line() +
  geom_point() +
  labs(
    title = "Error vs paràmetres C i gamma",
    color = "gamma"
  ) +
  theme_bw()
