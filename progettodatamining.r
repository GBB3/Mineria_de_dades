library(tidyverse)
library(psych)
library(ggplot2)
library(patchwork)

# Carica il dataset
df <- read.csv("train.csv")

# Variabili numeriche e categoriche
df[c("key", "audio_mode", "ID")] <- lapply(df[c("key", "audio_mode", "ID")], as.factor)
clases <- sapply(df, class)
varNum <- names(clases)[clases %in% c("numeric","integer")]
varCat <- names(clases)[which(clases %in% c("character", "factor"))]  # da trattare come categoriche
dim(df)           # dimensioni
str(df)           # tipi variabili
summary(df)       # riassunto statistico
colSums(is.na(df)) # NA per colonna

psych::describe(df[, varNum])

# Istogramma + densità per variabile
ggplot(df, aes(x=loudness)) + 
  geom_histogram(aes(y=..density..), bins=30, fill="skyblue") +
  geom_density(col="red") +
  geom_vline(aes(xintercept=mean(loudness, na.rm=TRUE)), col="blue", lty=2)


table(df$time_signature, useNA="ifany")
prop.table(table(df$key))
ggplot(df, aes(x=factor(audio_mode))) +
  geom_bar(fill="orange") +
  ylab("Frequenze")

library(PerformanceAnalytics)
chart.Correlation(df[, c("loudness","energy","danceability","song_popularity")], histogram=TRUE, pch=19)

psych::describeBy(df$loudness, df$time_signature)
ggplot(df, aes(x=factor(key), y=tempo)) + 
  geom_boxplot(fill="lightblue")

table(df$time_signature, df$audio_mode)

# skimr
library(skimr); skim(df)

# visdat
library(visdat); vis_dat(df); vis_miss(df)

# inspectdf
library(inspectdf); inspect_types(df); inspect_na(df); inspect_cor(df)

# dataReporter
library(dataReporter); makeDataReport(df)

# DataExplorer
library(DataExplorer); plot_str(df); plot_missing(df); plot_histogram(df); plot_correlation(df)

# esquisse
library(esquisse); esquisser(df)

varNum <- varNum[c(1:4, 6:10, 12, 14)]

pairs(df[, varNum])
num_df <- df[, varNum]
corr <- cor(num_df, use = "pairwise.complete.obs")

# =========================================================
# Advanced Preprocessing Pipeline (tidyverse + tidymodels)
# Dataset: train.csv (song features, target = song_popularity)
# =========================================================

# -----------------------
# 0) Setup & Parameters
# -----------------------
USE_MCAR_TEST      <- TRUE       # Little's MCAR test (richiede BaylorEdPsych)
IMPUTE_METHOD      <- "mice"     # "knn" | "mice" | "median"
APPLY_WINSORIZE    <- TRUE       # capping outlier univariati ai quantili
WINSOR_QUANTILES   <- c(0.01, 0.99)
REMOVE_MV_OUTLIERS <- FALSE      # rimuovere outlier multivariati (Mahalanobis)
MAHA_PVAL          <- 0.001      # soglia p-val per outlier (chi-quadro)
HIGH_CORR_CUTOFF   <- 0.9        # soglia r per rimozione variabili altamente correlate
SPLIT_VALID_PROP   <- 0.2        # train/valid split
SEED               <- 123

# -----------------------
# 1) Libraries
# -----------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(dlookr)         # diagnose/overview (diagnosi iniziale)
  library(naniar)         # visualizzazione pattern di NA
  library(tidymodels)     # recipes + rsample
  library(DescTools)      # Winsorize
  library(caret)          # findCorrelation, findLinearCombos, nearZeroVar
})
if (USE_MCAR_TEST) {
  suppressPackageStartupMessages(library(BaylorEdPsych))  # LittleMCAR
}
if (IMPUTE_METHOD == "mice") {
  suppressPackageStartupMessages(library(mice))           # imputazione multipla
}
if (IMPUTE_METHOD == "knn") {
  suppressPackageStartupMessages(library(VIM))            # kNN imputation
}

set.seed(SEED)

# -----------------------
# 2) Load data
# -----------------------
raw <- readr::read_csv("train.csv", show_col_types = FALSE)

# Identifica target e ID
target_col <- "song_popularity"
id_col     <- "ID"

stopifnot(target_col %in% names(raw))
stopifnot(id_col %in% names(raw))

# -----------------------
# 3) Diagnosi iniziale
# -----------------------
cat("\n==== DIAGNOSI INIZIALE ====\n")
print(dlookr::overview(raw))
print(dlookr::diagnose(raw))

# Conteggio NA per colonna
na_tbl <- raw %>% summarise(across(everything(), ~sum(is.na(.)))) %>% pivot_longer(everything(),
                                                                                   names_to = "feature", values_to = "n_na") %>% arrange(desc(n_na))
cat("\nNA per colonna (prime 10):\n"); print(head(na_tbl, 10))

# -----------------------
# 4) NA pattern + MCAR (opzionale)
# -----------------------
cat("\n==== PATTERN NA ====\n")
naniar::gg_miss_upset(raw) + ggtitle("Pattern di NA")  # apparirà in Plots

if (USE_MCAR_TEST) {
  # Little's MCAR test richiede solo numeriche complete (gestisce internamente NA)
  num_only <- raw %>% select(where(is.numeric))
  cat("\nLittle's MCAR test (solo numeriche):\n")
  suppressWarnings(print(BaylorEdPsych::LittleMCAR(num_only)))
}

# -----------------------
# 5) Outlier univariati (capping opzionale, senza DescTools)
# -----------------------
# Winsorization fatta a mano con quantili
winsorize_vec <- function(x, probs = c(0.01, 0.99)) {
  # Se tutto NA o meno di 2 non-NA, non toccare
  if (sum(!is.na(x)) < 2L) return(x)
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  x <- pmax(x, qs[1], na.rm = TRUE)
  x <- pmin(x, qs[2], na.rm = TRUE)
  return(x)
}

winsorize_df <- function(df, cols, probs = c(0.01, 0.99)) {
  df %>%
    mutate(across(all_of(cols), ~ winsorize_vec(.x, probs = probs)))
}

numeric_cols <- raw %>%
  select(where(is.numeric)) %>%
  select(-all_of(c(id_col, target_col))) %>%
  names()

df1 <- raw
if (APPLY_WINSORIZE) {
  df1 <- winsorize_df(df1, numeric_cols, WINSOR_QUANTILES)
  cat("\n[Outlier Uni] Winsorization applicata su numeriche (",
      paste0(WINSOR_QUANTILES * 100, collapse = "%, "), "% )\n", sep = "")
}

# -----------------------
# 6) Outlier multivariati (Mahalanobis) - flag & rimozione opzionale
# -----------------------
num_mat <- df1 %>% select(all_of(numeric_cols)) %>% as.matrix()
# Stima media e cov con complete cases (per robustezza si potrebbe usare covMcd)
cc_idx <- stats::complete.cases(num_mat)
mu <- colMeans(num_mat[cc_idx, , drop = FALSE], na.rm = TRUE)
S  <- stats::cov(num_mat[cc_idx, , drop = FALSE], use = "pairwise.complete.obs")

# Distanze di Mahalanobis (ignora righe con NA dopo winsorization comunque)
d2 <- stats::mahalanobis(x = num_mat, center = mu, cov = S)
p  <- ncol(num_mat)
cut <- stats::qchisq(1 - MAHA_PVAL, df = p)
mv_outlier <- ifelse(is.finite(d2) & d2 > cut, 1L, 0L)

df1 <- df1 %>% mutate(.mv_outlier = mv_outlier)

cat("\n[Outlier Multi] Righe flaggate:", sum(df1$.mv_outlier == 1, na.rm = TRUE),
    " su ", nrow(df1), " (soglia p<", MAHA_PVAL, ")\n", sep = "")

if (REMOVE_MV_OUTLIERS) {
  df1 <- df1 %>% filter(.mv_outlier == 0L)
  cat("[Outlier Multi] Rimosse righe outlier multivariati. Nuove dimensioni:",
      nrow(df1), "x", ncol(df1), "\n")
}

# -----------------------
# 7) Imputazione valori mancanti
# -----------------------
df2 <- df1 %>% select(-.mv_outlier)  # togli flag dalla fase successiva

if (IMPUTE_METHOD == "median") {
  # Imputa numeriche con mediana, categoriche (se presenti) con moda
  mode_fun <- function(x) { ux <- unique(x[!is.na(x)]); ux[which.max(tabulate(match(x, ux)))] }
  df2 <- df2 %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
    mutate(across(where(is.character), ~ifelse(is.na(.), mode_fun(.), .)))
  cat("\n[Imputazione] Metodo = MEDIAN/MODE\n")
  
} else if (IMPUTE_METHOD == "knn") {
  # VIM::kNN lavora bene se le categoriche sono factor
  df_knn <- df2 %>% mutate(across(where(is.character), as.factor))
  knn_res <- VIM::kNN(df_knn, k = 5, imp_var = FALSE)
  df2 <- as_tibble(knn_res)
  cat("\n[Imputazione] Metodo = KNN (k=5)\n")
  
} else if (IMPUTE_METHOD == "mice") {
  # Solo colonne numeriche + (eventuali) categoriche come factor
  df_m <- df2 %>%
    mutate(across(where(is.character), as.factor))
  
  # mice non vuole colonne con varianza nulla o tutte NA: le isoliamo e reintegriamo
  all_na_cols <- names(df_m)[sapply(df_m, function(x) all(is.na(x)))]
  if (length(all_na_cols) > 0) {
    warning("Colonne solo NA rimosse temporaneamente: ", paste(all_na_cols, collapse=", "))
  }
  df_m_core <- df_m %>% select(-all_of(all_na_cols))
  
  imp <- mice::mice(df_m_core, m = 5, maxit = 10, method = "pmm", seed = SEED, printFlag = FALSE)
  df_comp <- mice::complete(imp, action = "long") %>%
    group_by(.id) %>%
    summarise(across(where(~!is.numeric(.x) || is.numeric(.x)), ~{
      if (is.numeric(.)) mean(., na.rm = TRUE) else names(sort(table(.), decreasing = TRUE))[1]
    }), .groups = "drop")
  
  # Rimettiamo le colonne all-NA (rimangono NA; valuta se dropparle)
  for (cn in all_na_cols) df_comp[[cn]] <- NA
  # Ripristina ordine colonne
  df2 <- df_comp %>% select(all_of(names(df_m)), everything()) %>% select(names(df_m))
  cat("\n[Imputazione] Metodo = MICE (pmm, m=5)\n")
  
} else {
  stop("IMPUTE_METHOD deve essere uno tra: 'median', 'knn', 'mice'")
}

# -----------------------
# Report post-imputazione (Step 7)
# -----------------------

# Quante NA c’erano prima (df1) e dopo (df2)
na_before <- colSums(is.na(df1))
na_after  <- colSums(is.na(df2))

cat("\n=== REPORT IMPUTAZIONE ===\n")
report <- tibble(
  feature   = names(na_before),
  NA_before = na_before,
  NA_after  = na_after,
  Imputed   = NA_before - NA_after
)

print(report %>% arrange(desc(Imputed)) %>% head(15))

# Statistiche delle variabili imputate (esempio su prime 5 con Imputed > 0)
imp_feats <- report %>% filter(Imputed > 0) %>% pull(feature)

if (length(imp_feats) > 0) {
  cat("\nEsempio statistiche su variabili imputate:\n")
  print(df2 %>%
          select(all_of(head(imp_feats, 5))) %>%
          summary())
} else {
  cat("\nNessuna variabile aveva NA, quindi nessuna imputazione è stata fatta.\n")
}


# -----------------------
# 8) Feature Selection: zero variance, linear combos, alta correlazione
# -----------------------
# (a) Zero / near-zero variance
nzv_idx <- caret::nearZeroVar(df2 %>% select(-all_of(c(id_col, target_col))), saveMetrics = TRUE)
nzv_drop <- rownames(nzv_idx)[nzv_idx$nzv | nzv_idx$zeroVar]
if (length(nzv_drop) > 0) cat("\n[NZV] Rimuovo:", paste(nzv_drop, collapse = ", "), "\n")

df3 <- df2 %>% select(-all_of(nzv_drop))

# (b) Linear combinations
num_for_lc <- df3 %>% select(where(is.numeric), -all_of(c(id_col, target_col)))
if (ncol(num_for_lc) > 1) {
  lc <- caret::findLinearCombos(as.matrix(num_for_lc))
  lc_drop <- if (!is.null(lc$remove)) colnames(num_for_lc)[lc$remove] else character(0)
} else lc_drop <- character(0)
if (length(lc_drop) > 0) cat("[Linear Combos] Rimuovo:", paste(lc_drop, collapse = ", "), "\n")

df3 <- df3 %>% select(-all_of(lc_drop))

# (c) High correlation
num_for_corr <- df3 %>% select(where(is.numeric), -all_of(c(id_col, target_col)))
if (ncol(num_for_corr) > 1) {
  cor_mat <- stats::cor(num_for_corr, use = "pairwise.complete.obs")
  high_corr_idx <- caret::findCorrelation(cor_mat, cutoff = HIGH_CORR_CUTOFF, names = TRUE)
} else high_corr_idx <- character(0)
if (length(high_corr_idx) > 0) cat("[High Corr] Rimuovo:", paste(high_corr_idx, collapse = ", "), "\n")

df4 <- df3 %>% select(-all_of(high_corr_idx))

# ---------------------
# ---- Step 9: Split train/valid (strata = target continuo) ----
# ---------------------
set.seed(SEED)
split <- initial_split(df4, prop = 1 - SPLIT_VALID_PROP, strata = !!sym(target_col))
train <- training(split)
valid <- testing(split)
cat("\nSplit: train =", nrow(train), " | valid =", nrow(valid), "\n")


# -----------------------
# 10) Ricetta: scaling, trasformazioni (opzionale)
# -----------------------
rec <- recipe(as.formula(paste(target_col, "~ .")), data = train) %>%
  update_role(all_of(id_col), new_role = "id") %>%
  step_rm(all_of(id_col)) %>%
  step_normalize(all_numeric_predictors()) %>%        # standardizzazione
  step_YeoJohnson(all_numeric_predictors())           # riduce skew/outlier morbidi

# Prepara e applica
prep_rec <- prep(rec)
train_proc <- bake(prep_rec, new_data = NULL)
valid_proc <- bake(prep_rec, new_data = valid)

# -----------------------
# 11) Export
# -----------------------
dir.create("processed", showWarnings = FALSE)
readr::write_csv(train_proc, "processed/train_processed.csv")
readr::write_csv(valid_proc, "processed/valid_processed.csv")

cat("\n=== DONE ===\n")
cat("File salvati in ./processed/:\n - train_processed.csv\n - valid_processed.csv\n")

# -----------------------
# 12) Report rapido post-preprocess
# -----------------------
cat("\nPOST-PROCESS CHECK (train):\n")
print(dlookr::overview(train_proc))
print(dlookr::diagnose(train_proc))

