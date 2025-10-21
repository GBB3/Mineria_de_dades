
##############
# OUTLIERS ##
#############
load("df_all_imputed.RData")

#install.packages("mvoutlier")
library(ggplot2)


##########################################################
######################## UNIVARIANT #####################
#########################################################

## MAX, MIN

df_identifiers <- df_all_imputed[,c("global_ID","ID","dataset")]
df_all <- subset(df_all_imputed, select = -c(global_ID, ID, dataset))


clases <- sapply(df_all, class)
varNum <- names(clases)[which(clases %in% c("numeric", "integer"))]
varCat <- names(clases)[which(clases %in% c("character", "factor"))]

mapply(function(x, name) {
  cat("var. ", name, ": \n\t min: ", min(x), "\n\t max: ", max(x), "\n")
  invisible(NULL)  # Evita la salida de valores NULL
}, df_all[, varNum], colnames(df_all[, varNum]))

## IQR

library(EnvStats)

IQROutlier <- function(variable, rmnas = TRUE) {
  IQ <- iqr(variable, na.rm = rmnas)
  intInf <- quantile(variable, probs = c(0.25, 0.75))[[1]] - 1.5*IQ
  intSup <- quantile(variable, probs = c(0.25, 0.75))[[2]] + 1.5*IQ
  posicions <- which(variable >= intSup | variable <= intInf)
  if (length(posicions) > 0) {
    cat("Existeixen outliers en les posicions:", paste0(posicions, collapse = ", "))
  } else {
    cat("No existeixen outliers")
  }
  return(posicions)
}

outliers_list <- lapply(varNum, function(var) {
  cat("\nVariable:", var, "\n")
  IQROutlier(df_all[[var]])  
})

names(outliers_list) <- varNum
  
## BOXPLOT
# Crear un boxplot
ggplot(df_all, aes(y = get(variable))) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = paste0("Boxplot de ", variable)) +
  theme_minimal()

boxplot.stats(df_all[, variable])$out

## Z-SCORE
variable <- "Age"
valorEscalado <- scale(df_all[, variable])
hist(valorEscalado)

ggplot(data.frame(valor = valorEscalado), aes(x = valor)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +  # Histograma
  geom_vline(xintercept = c(3, -3), linetype = "dashed", color = "red", size = 1) + # Líneas horizontales
  theme_minimal()

for (n in varNum) {
  valorEscalado <- scale(df_all[[n]])
  
  p <- ggplot(data.frame(valor = valorEscalado), aes(x = valor)) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
    geom_vline(xintercept = c(3, -3), linetype = "dashed", color = "red", size = 1) +
    labs(title = paste("Distribució Z-score de", n),
         x = "Z-score", y = "Freqüència") +
    theme_minimal()
  
  print(p)
}

## HAMPEL IDENTIFIER

variable <- "Age"

lower_bound <- median(df_all[, variable]) - 3 * mad(df_all[, variable], constant = 1)
upper_bound <- median(df_all[, variable]) + 3 * mad(df_all[, variable], constant = 1)
outlier_ind <- which((df_all[, variable] < lower_bound) | (df_all[, variable] > upper_bound))
outlier_ind

hampel_outliers_df <- data.frame(
  variable = character(),
  valor_outlier = numeric(),
  ID = character(),
  stringsAsFactors = FALSE
)

for (n in varNum) {
  x <- df_all[[n]]
  
  lower_bound <- median(x, na.rm = TRUE) - 3 * mad(x, constant = 1, na.rm = TRUE)
  upper_bound <- median(x, na.rm = TRUE) + 3 * mad(x, constant = 1, na.rm = TRUE)
  
  outlier_ind <- which(x < lower_bound | x > upper_bound)
  
  if (length(outlier_ind) > 0) {
    
    temp <- data.frame(
      variable = n,
      valor_outlier = x[outlier_ind],
      ID = df_all$ID[outlier_ind] 
    )
    hampel_outliers_df <- rbind(hampel_outliers_df, temp)
  }
}

hampel_outliers_df

## STADISTICAL TESTS
### GRUBS
### DIXON'S
### ROSNER'S


##########################################################
######################## MULTIVARIANT #####################
#########################################################
## GENERAL CASE
library(mvoutlier)
df_all2 <- df_all; Y <- as.matrix(df_all2)
distances <- dd.plot(Y,quan=1/2, alpha=0.025)

head(distances$md.cla)
head(distances$md.rob)

res <- aq.plot(Y,delta=qchisq(0.975,df=ncol(Y)),quan=1/2,alpha=0.05)
str(res)
head(res$outliers)
table(res$outliers)

#windows()
par(mfrow=c(1, 1))
library(MVN)
# mvnoutliers <- mvn(df_all, multivariateOutlierMethod = "adj", showOutliers = TRUE, 
#                   showNewData = TRUE)
mvnoutliers <- mvn(data = df_all, mvn_test = "royston", 
                   univariate_test = "AD", 
                   multivariate_outlier_method = "adj",
                   show_new_data = TRUE)
head(summary(mvnoutliers, select = "outliers"))
head(summary(mvnoutliers, select = "new_data"))
head(summary(mvnoutliers, select = "mvn"))
head(summary(mvnoutliers, select = "univariate"))

## PCA
## MAHALANOBIS DISTANCE

distancia_mahalanobis <- mahalanobis(df_all, colMeans(df_all), cov(df_all))
plot(density(distancia_mahalanobis))

cutoff <- qchisq(p = 0.99, df = ncol(df_all))
df_all[distancia_mahalanobis>cutoff, ]

df_all <- df_all[order(distancia_mahalanobis, decreasing = TRUE),]
par(mfrow=c(1,1))
hist(distancia_mahalanobis)

umbral <- 8
df_all[, "outlier"] <- (distancia_mahalanobis > umbral)

df_all[, "color"] <- ifelse(df_all[, "outlier"], "red", "black")
scatterplot3d(df_all[, "DC"], df_all[, "temp"], df_all[, "RH"], 
              color = df_all[, "color"])

(fig <- plotly::plot_ly(df_all, x = ~DC, y = ~temp, z = ~RH, 
                        color = ~color, colors = c('#0C4B8E', '#BF382A')) %>% 
    add_markers())
(quienes <- which(df_all[, "outlier"] == TRUE))

### ROBUST MAHALANOBLIS 
library(chemometrics)

dis <- chemometrics::Moutlier(df_all[, c("DC", "temp", "RH")], quantile = 0.99, plot = TRUE)

par(mfrow = c(1, 1))
plot(dis$md, dis$rd, type = "n")
text(dis$md, dis$rd, labels = rownames(df_all))

a <- which(dis$rd > 7)
print(a)

## REGRESION
## COOK'S DISTANCE
## KNN
library(adamethods)

do_knno(df_all[, c("DC", "temp", "RH")], k=1, top_n = 30)

## LOF

library(DMwR2)
library(dplyr)

outlier.scores <- lofactor(df_all[, c("DC", "temp", "RH")], k = 5)
par(mfrow=c(1,1))
plot(density(outlier.scores))
outlier.scores
outliers <- order(outlier.scores, decreasing=T)
outliers <- order(outlier.scores, decreasing=T)[1:5]

n <- nrow(df_all[, c("DC", "temp", "RH")]); labels <- 1:n; labels[-outliers] <- "."
biplot(prcomp(df_all[, c("DC", "temp", "RH")]), cex = .8, xlabs = labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(df_all[, c("DC", "temp", "RH")], pch = pch, col = col)

plot3d(df_all[, "DC"], df_all[, "temp"], df_all[, "RH"], type = "s", col = col, size = 1)

### NEW LOF
library(Rlof)
outliers.scores <- Rlof::lof(df_all[, c("DC", "temp", "RH")], k = 5)
plot(density(outliers.scores))

#outlier.scores <- lof(df_all[, c("DC", "temp", "RH")], k=c(5:10))

## ISOLATION FOREST
### Cargamos las librerias necesarias
library(R.matlab)   # Lectura de archivos .mat
library(solitude)   # Modelo isolation forest
library(tidyverse)  # Preparación de datos y gráficos
library(MLmetrics)

# Carreguem les dades
cardio_mat  <- readMat("https://www.dropbox.com/s/galg3ihvxklf0qi/cardio.mat?dl=1")
df_cardio   <- as.data.frame(cardio_mat$X)
df_cardio$y <- as.character(cardio_mat$y)
datos <- df_cardio

isoforest <- isolationForest$new(
  sample_size = as.integer(nrow(datos)/2),
  num_trees   = 500, 
  replace     = TRUE,
  seed        = 123
)
isoforest$fit(dataset = datos %>% select(-y))

predicciones <- isoforest$predict(
  data = datos %>% select(-y)
)
head(predicciones)

ggplot(data = predicciones, aes(x = average_depth)) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = quantile(predicciones$average_depth, seq(0, 1, 0.1)),
    color      = "red",
    linetype   = "dashed") +
  labs(
    title = "Distribución de las distancias medias del Isolation Forest",
    subtitle = "Cuantiles marcados en rojo"  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 11))

cuantiles <- quantile(x = predicciones$average_depth, probs = seq(0, 1, 0.05))
cuantiles



