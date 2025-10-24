##############
# OUTLIERS ##
#############

## PAQUETS:
load("df_all_imputed.RData")

#install.packages("mvoutlier")
#install.packages("EnvStats")

dir.create("plots OUTLIERS", showWarnings = FALSE)

## DATASETS:
df_identifiers <- df_all_imputed[,c("global_ID","ID","dataset","song_popularity")]
df_all <- subset(df_all_imputed, select = -c(global_ID, ID, dataset,song_popularity))

clases <- sapply(df_all, class)
varNum <- names(clases)[which(clases %in% c("numeric", "integer"))]
varCat <- names(clases)[which(clases %in% c("character", "factor"))]

df_num <- df_all[,varNum]

########################
## TEST DE NORMALITAT ##
#######################
library(ggplot2)
library(dplyr)
library(tidyr)

# Crear un dataframe per guardar resultats KS
results_KS <- data.frame(
  Variable = character(),
  D_statistic = numeric(),
  p_value = numeric(),
  Normal = logical(),
  stringsAsFactors = FALSE
)

# Carpeta per guardar els QQ-plots
dir.create("QQ_plots", showWarnings = FALSE)

for (var in names(df_num)) {
  x <- df_num[[var]]
  
  # KS-test comparant amb distribució normal amb mateixa mitjana i sd
  if (length(x) >= 3) {
    test <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
    
    results_KS <- rbind(
      results_KS,
      data.frame(
        Variable = var,
        D_statistic = test$statistic,
        p_value = test$p.value,
        Normal = test$p.value > 0.05
      )
    )
    
    # Generar QQ-plot amb ggplot2
    qq_plot <- ggplot(data.frame(x = x), aes(sample = x)) +
      stat_qq() +
      stat_qq_line(col = "red") +
      labs(title = paste("QQ-plot de", var)) +
      theme_minimal()
    
    # Guardar plot
    ggsave(filename = paste0("QQ_plots/QQ_", var, ".png"), plot = qq_plot, width = 5, height = 4)
  }
}

# Ordenar resultats per p-value
results_KS <- results_KS[order(results_KS$p_value),]
print(results_KS)


# Mostrem els resultats
print(results_normalitat)

# Opcional: ordenar per p-value (de menys normal a més normal)
results_normalitat <- results_normalitat[order(results_normalitat$p_value),]
print(results_normalitat)

# No hi ha cap normal

##########################################################
######################## UNIVARIANT #####################
#########################################################

## MAX, MIN

mapply(function(x, name) {
  cat("var. ", name, ": \n\t min: ", min(x), "\n\t max: ", max(x), "\n")
  invisible(NULL)  # Evita la salida de valores NULL
}, df_all[, varNum], colnames(df_all[, varNum]))

# loudness: usual max value is 0. Max is 1.58 aceptable but rare. 
# song_duration: max 1799346 = 30 min. It is too much.
# tempo: it is not possible for a song to have 0 BPM.

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
str(outliers_list)
  
## BOXPLOT
# Crear un boxplot
ggplot(df_all, aes(y = get(variable))) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = paste0("Boxplot de ", variable)) +
  theme_minimal()

boxplot.stats(df_all[, variable])$out

## Z-SCORE (Només per variables normals)
main_dir <- file.path("plots OUTLIERS", "Z-SCORE")
dir.create(main_dir, showWarnings = FALSE)

for (var in varNum) {
  subdir <- file.path(main_dir, var)
  dir.create(subdir, showWarnings = FALSE)
  
  valorEscalado <- scale(df_all[[var]])
  
  histo <- ggplot(data.frame(valor = valorEscalado), aes(x = valor)) +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
    geom_vline(xintercept = c(3, -3), linetype = "dashed", color = "red", size = 1) +
    labs(title = paste("Distribució Z-score de", var),
         x = "Z-score", y = "Freqüència") +
    theme_minimal()
  ggsave(filename = file.path(subdir, paste0("histograma_", var, ".png")), plot = histo)
}

## HAMPEL IDENTIFIER

hampel_outliers_df <-data.frame()
for (var in varNum) {
  x <- df_all[[var]]
  
  lower_bound <- median(x, na.rm = TRUE) - 3 * mad(x, constant = 1, na.rm = TRUE)
  upper_bound <- median(x, na.rm = TRUE) + 3 * mad(x, constant = 1, na.rm = TRUE)
  
  outlier_ind <- which(x < lower_bound | x > upper_bound)
  
  if (length(outlier_ind) > 0) {
    
    temp <- data.frame(
      variable = var,
      valor_outlier = x[outlier_ind],
      ID = df_identifiers$ID[outlier_ind]  # canviar dataframe
    )
    hampel_outliers_df <- rbind(hampel_outliers_df, temp)
  }
}

head(hampel_outliers_df)

## STADISTICAL TESTS
### GRUBS
### DIXON'S
### ROSNER'S


##########################################################
######################## MULTIVARIANT #####################
#########################################################

## GENERAL CASE
library(mvoutlier)
df_all2 <- df_all[,varNum]; Y <- as.matrix(df_all2)
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

library(scatterplot3d)
library(plotly)

distancia_mahalanobis <- mahalanobis(df_num, colMeans(df_num), cov(df_num))
plot(density(distancia_mahalanobis))

# We set the cutoff at a 0.99 level
cutoff <- qchisq(p = 0.99, df = ncol(df_num))
df_num[distancia_mahalanobis>cutoff, ]

df_mahalanobis <- df_num[order(distancia_mahalanobis, decreasing = TRUE),]
par(mfrow=c(1,1))
hist(distancia_mahalanobis, breaks = 50, 
     main = "Distribució de distàncies de Mahalanobis",
     xlab = "Distància", col = "skyblue", border = "white") 
max(distancia_mahalanobis) # we have some big values = extrem outliers 

df_mahalanobis[, "outlier"] <- (distancia_mahalanobis > cutoff)

# We choose three numerical variables to visualice the distribution of the outlier across the variables.
df_mahalanobis[, "color"] <- ifelse(df_mahalanobis[, "outlier"], "red", "black")
scatterplot3d(df_mahalanobis[, "danceability"], df_mahalanobis[, "energy"], df_mahalanobis[, "tempo"], 
              color = df_mahalanobis[, "color"])

(fig <- plotly::plot_ly(df_mahalanobis, x = ~danceability, y = ~energy, z = ~tempo, 
                        color = ~color, colors = c('#0C4B8E', '#BF382A')) %>% 
    add_markers())
(quienes <- which(df_mahalanobis[, "outlier"] == TRUE))

### ROBUST MAHALANOBLIS 
#install.packages("chemometrics")
library(chemometrics)
estandard<-scale(df_num,)

dis <- chemometrics::Moutlier(estandard, quantile = 0.99, plot = TRUE)
umbr_md <- quantile(dis$md, 0.99)
umbr_rd <- quantile(dis$rd, 0.99)

par(mfrow = c(1, 1))
plot(dis$md, dis$rd, type = "n")
text(dis$md, dis$rd, labels = rownames(df_num))

plot(dis$md, dis$rd)
abline(v=umbr_md, col="red",lty= 2)
abline(h=umbr_rd,col="red",lty= 2)


a <- which(dis$md > umbr_md | dis$rd > umbr_rd)
num.elim.r<-sum(dis$md > umbr_md | dis$rd > umbr_rd) 
num.elim.r

## REGRESION
## COOK'S DISTANCE
## KNN
library(adamethods)

do_knno(df_num[, c("DC", "temp", "RH")], k=1, top_n = 30)

## LOF

#install.packages("DMwR2")
library(DMwR2)
library(dplyr)

outlier.scores <- lofactor(df_num, k = 5)
par(mfrow=c(1,1))
plot(density(outlier.scores))
outlier.scores

# Ver outliers
outliers.lof <- which(outlier.scores > quantile(outlier.scores, 0.99))


length(outliers.lof)
outliers <- order(outlier.scores, decreasing=T)
outliers <- order(outlier.scores, decreasing=T)[1:5]


n <- nrow(df_num[, c("DC", "temp", "RH")]); labels <- 1:n; labels[-outliers] <- "."
biplot(prcomp(df_num[, c("DC", "temp", "RH")]), cex = .8, xlabs = labels)

pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(df_num[, c("DC", "temp", "RH")], pch = pch, col = col)

plot3d(df_num[, "DC"], df_num[, "temp"], df_num[, "RH"], type = "s", col = col, size = 1)

### NEW LOF
#install.packages("Rlof")
library(Rlof)
outliers.scores <- Rlof::lof(df_num, k = 5) ## !!!! probar més k, recomat provar valors més grans quan tenim tantes obs
plot(density(outliers.scores), 
     main="Distribución de LOF Scores con Rlof",
     col="blue", lwd=2, xlim=c(0, max(outliers.scores, na.rm = TRUE) * 1.1))
abline(v=quantile(outliers.scores, 0.99), col="red", lty=2)

outliers.lof <- which(outliers.scores > quantile(outliers.scores, 0.99))
length(outliers.lof)

#outlier.scores <- lof(df_num[, c("DC", "temp", "RH")], k=c(5:10))

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

# UNIVARIANT
# Min i maxim: molt simple. Sensible escala.
# IQR: no requereix normalitat però amb dades asimetriques pot fallar.
# Z-Score: requereix aproximació a la normalitat i amb dades asimètriques pot fallar.
# Hampel identifier: robust davant de distribucions asimètriques. Al ser univariant no captura relacions entre variables.

# BIVARIANT
# Mahalanobis: suposa aproximació normal multivariant. I sensible a outliers extrems.
# Mahalanobis robust: millora mahalanobis i és menys sensible a outliers extrems.
# LOF: no assumeix normalitat ni simètria. 

#### Escollir el LOF (comparar LOF amb newLOF)
#### Tractar com varible dummy ja que tenim variables amb una gran proporció d'outliers com speechiness i instrumentaless. 

# Crear variable indicador de outlier
df_num$is_outlier <- FALSE
df_num$is_outlier[outliers.lof] <- TRUE
df_num

library(psych)

describe(df_num[outliers.lof,])
par(mfrow=c(6,4))

describe(df_num[outliers.lof,])
describe(df_num[-outliers.lof,])
