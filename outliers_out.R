## PAQUETS:
load("df_all_imputed_no_duplicates.RData")

#install.packages("mvoutlier")
#install.packages("EnvStats")

## DATASETS:
df_identifiers <- df_all_imputed[,c("global_ID","ID","dataset","song_popularity")]
df_all <- subset(df_all_imputed, select = -c(global_ID, ID, dataset,song_popularity))
df_all_imputed_test <- df_all_imputed[df_all_imputed$dataset=="test",]

clases <- sapply(df_all, class)
varNum <- names(clases)[which(clases %in% c("numeric", "integer"))]
varCat <- names(clases)[which(clases %in% c("character", "factor"))]

df_cat <- df_all[, varCat]
df_num <- df_all[,varNum]

################################################################################
########## NO OUTLIERS EXTREMS/DUMMIES/KEY_TRIGO ###################
################################################################################
## IQR Extrem

library(EnvStats)

IQROutlier <- function(variable, rmnas = TRUE) {
  IQ <- iqr(variable, na.rm = rmnas)
  intInf <- quantile(variable, probs = c(0.25, 0.75))[[1]] - 3*IQ
  intSup <- quantile(variable, probs = c(0.25, 0.75))[[2]] + 3*IQ
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

# 1. Unir tots els outliers de totes les variables (posicions)
all_outlier_positions <- unique(unlist(outliers_list))

# 2. Crear la columna IQR_extrem al dataframe original
df_all_imputed$IQR_extrem <- ifelse(
  seq_len(nrow(df_all_imputed)) %in% all_outlier_positions,
  1, 
  0
)

## IQR Mid

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

# 1. Unir tots els outliers de totes les variables (posicions)
all_outlier_positions <- unique(unlist(outliers_list))

# 2. Crear la columna IQR_extrem al dataframe original
df_all_imputed$IQR_mid <- ifelse(
  seq_len(nrow(df_all_imputed)) %in% all_outlier_positions,
  1, 
  0
)

df_all_imputed$IQRmid_no_extrem <- NA

for (i in 1:nrow(df_all_imputed)) {
  if (df_all_imputed$IQR_extrem[i] == df_all_imputed$IQR_mid[i]) {
    df_all_imputed$IQRmid_no_extrem[i] <- 0
  } else {
    df_all_imputed$IQRmid_no_extrem[i] <- 1
  }
}

table(df_all_imputed$IQR_extrem)
table(df_all_imputed$IQR_mid)
table(df_all_imputed$IQRmid_no_extrem)

# Treiem outliers univariants
df_no_outliers_extrems <- df_all_imputed[df_all_imputed$IQR_extrem == 0 & df_all_imputed$dataset == "train" ,]
df_no_outliers_extrems <- subset(df_no_outliers_extrems,select=-c(IQR_extrem,IQR_mid,IQRmid_no_extrem))
df_no_outliers_extrems <- rbind(df_all_imputed_test,df_no_outliers_extrems)


# Treiem outliers multivariant

# Transformem categoriques en dummies
table(df_no_outliers_extrems$key)
table(df_no_outliers_extrems$audio_mode)

#install.packages("fastDummies")
library(fastDummies)
df_no_outliers_extrems_dummies <- fastDummies::dummy_cols(df_no_outliers_extrems,
                                  select_columns = c("key"),
                                  remove_first_dummy = TRUE)
df_no_outliers_extrems_dummies$key <- NULL

#Transformem key trigonometricament
df_no_outliers_extrems_keytrigo <- df_no_outliers_extrems
df_no_outliers_extrems_keytrigo$key_sin <- sin(2*pi*as.numeric(df_no_outliers_extrems_keytrigo$key)/12)
df_no_outliers_extrems_keytrigo$key_cos <- cos(2*pi*as.numeric(df_no_outliers_extrems_keytrigo$key)/12)
df_no_outliers_extrems_keytrigo$key <- NULL

# Guardem 
save(df_no_outliers_extrems, file="df_no_outliers_extrems.RData")
save(df_no_outliers_extrems_dummies, file="df_no_outliers_extrems_dummies.RData")
save(df_no_outliers_extrems_keytrigo, file="df_no_outliers_extrems_keytrigo.RData")

################################################################################
########## NO OUTLIERS EXTREMS UNIVARIANTS/DUMMIES/KEY_TRIGO ###################
################################################################################
## IQR Extrem
library(EnvStats)

IQROutlier <- function(variable, rmnas = TRUE) {
  IQ <- iqr(variable, na.rm = rmnas)
  intInf <- quantile(variable, probs = c(0.25, 0.75))[[1]] - 3*IQ
  intSup <- quantile(variable, probs = c(0.25, 0.75))[[2]] + 3*IQ
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

outliers_list$instrumentalness <- NULL
str(outliers_list)

# 1. Unir tots els outliers de totes les variables (posicions)
all_outlier_positions <- unique(unlist(outliers_list))

# 2. Crear la columna IQR_extrem al dataframe original
df_all_imputed$IQR_extrem_no_intrumentalness <- ifelse(
  seq_len(nrow(df_all_imputed)) %in% all_outlier_positions,
  1, 
  0
)

df_no_outliers_extrems_no_instrumetalness <- df_all_imputed[df_all_imputed$IQR_extrem_no_intrumentalness == 0 & df_all_imputed$dataset == "train" ,]
df_no_outliers_extrems_no_instrumetalness <- subset(df_no_outliers_extrems_no_instrumetalness,select=-c(IQR_extrem,IQR_mid,IQRmid_no_extrem,IQR_extrem_no_intrumentalness))
df_no_outliers_extrems_no_instrumetalness <- rbind(df_all_imputed_test,df_no_outliers_extrems_no_instrumetalness)

# Treiem outliers multivariant

#install.packages("fastDummies")
library(fastDummies)
df_no_outliers_extrems_no_instrumentalness_dummies <- fastDummies::dummy_cols(df_no_outliers_extrems,
                                                          select_columns = c("key"),
                                                          remove_first_dummy = TRUE)
df_no_outliers_extrems_no_instrumentalness_dummies$key <- NULL


#Transformem key trigonometricament
df_no_outliers_extrems__no_instrumentalness_keytrigo <- df_no_outliers_extrems_no_instrumetalness
df_no_outliers_extrems__no_instrumentalness_keytrigo$key_sin <- sin(2*pi*as.numeric(df_no_outliers_extrems__no_instrumentalness_keytrigo$key)/12)
df_no_outliers_extrems__no_instrumentalness_keytrigo$key_cos <- cos(2*pi*as.numeric(df_no_outliers_extrems__no_instrumentalness_keytrigo$key)/12)
df_no_outliers_extrems__no_instrumentalness_keytrigo$key <- NULL

# Guardem 
save(df_no_outliers_extrems_no_instrumetalness, file="df_no_outliers_extrems_no_instrumetalness.RData")
save(df_no_outliers_extrems_no_instrumentalness_dummies, file="df_no_outliers_extrems_no_instrumentalness_dummies.RData")
save(df_no_outliers_extrems_no_instrumentalness_dummies, file="df_no_outliers_extrems_no_instrumentalness_dummies.RData")
