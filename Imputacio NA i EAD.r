install.packages("mice")
install.packages("tidyverse")
library(tidyverse)

#---- creation unique dataset----

#load both datasets
df_tr <- read.csv("train.csv")
df_ts <- read.csv("test.csv")

head(df_tr)
head(df_ts)

# add the col "dataset" to both df
df_tr$dataset <- "train"
df_ts$dataset <- "test"

# add col  target to test.csv so the number of cols are the same: we put the value at 0
df_ts$song_popularity <- NA

# reorder of col names of test so they are same order of train
df_ts <- df_ts[, names(df_tr)]

## Factors
df_tr[c("ID", "time_signature", "key", "audio_mode","dataset")] <-
  lapply(df_tr[c("ID", "time_signature", "key", "audio_mode","dataset")], as.factor)
df_ts[c("ID", "time_signature", "key", "audio_mode","dataset")] <-
  lapply(df_ts[c("ID", "time_signature", "key", "audio_mode","dataset")], as.factor)

# combined the datasets
df_all <- rbind(df_tr, df_ts)

# final check
table(df_all$dataset)
head(df_all)

# check if all ID are unique (NB both singular datasets have ID that starts from 1 and go on..)
df_all$global_ID <- ifelse(df_all$dataset == "train",
                           paste0("train_", df_all$ID),
                           paste0("test_", df_all$ID))

length(unique(df_all$global_ID)) == nrow(df_all)



##### EAD

datos<-df_all
clases <- sapply(datos, class)
varNum <- names(clases)[which(clases %in% c("numeric", "integer"))]
varCat <- names(clases)[which(clases %in% c("character", "factor"))]


### NUMERICAL VARIABLES
library(psych)
psych::describe(datos[, varNum])

library(ggplot2)
library(patchwork)

# Crea la carpeta principal on es guardaran totes les imatges
dir.create("plots", showWarnings = FALSE)

# Bucle per a cada variable numèrica
for (var in varNum) {
  
  # Crea una subcarpeta per a cada variable dins de "plots"
  subdir <- file.path("plots", var)
  dir.create(subdir, showWarnings = FALSE)
  
  # Histograma
  histo <- ggplot(datos, aes(x = .data[[var]])) + 
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "#FF6666") +
    geom_vline(aes(xintercept = mean(.data[[var]], na.rm = TRUE)),
               color = "blue", linetype = "dashed", linewidth = 1) +
    ggtitle(paste("Histograma de", var))
  
  # Boxplot
  boxp <- ggplot(datos, aes(x = .data[[var]])) + 
    geom_boxplot(outlier.colour = "red", outlier.shape = 8,
                 outlier.size = 4) +
    ggtitle(paste("Boxplot de", var))
  
  # Desa les imatges a la subcarpeta
  ggsave(filename = file.path(subdir, paste0("histograma_", var, ".png")), plot = histo)
  ggsave(filename = file.path(subdir, paste0("boxplot_", var, ".png")), plot = boxp)
}

#### Categorical 

for (var in varCat) {
  tablaAbs <- data.frame(table(datos[, var]))
  tablaFreq <- data.frame(table(datos[, var])/sum(table(datos[, var])))
  m <- match(tablaAbs$Var1, tablaFreq$Var1)
  tablaAbs[, "FreqRel"] <- tablaFreq[m, "Freq"]
  colnames(tablaAbs) <- c("Categoria", "FreqAbs", "FreqRel")
  
  cat("===============", var, "===================================\n")
  print(tablaAbs)
  cat("==================================================\n")
}


library(ggplot2)
library(gridExtra)

# Crea la carpeta principal on es guardaran tots els gràfics de variables categòriques
dir.create("plots_cat", showWarnings = FALSE)

# Primer bucle: taules de freqüències absolutes i relatives
for (var in varCat) {
  tablaAbs <- data.frame(table(datos[, var]))
  tablaFreq <- data.frame(table(datos[, var]) / sum(table(datos[, var])))
  m <- match(tablaAbs$Var1, tablaFreq$Var1)
  tablaAbs[, "FreqRel"] <- tablaFreq[m, "Freq"]
  colnames(tablaAbs) <- c("Categoria", "FreqAbs", "FreqRel")
  
  cat("===============", var, "===================================\n")
  print(tablaAbs)
  cat("==================================================\n")
}

# Segon bucle: creació i desament dels gràfics
for (var in varCat) {
  # Crea una subcarpeta per a cada variable categòrica
  subdir <- file.path("plots_cat", var)
  dir.create(subdir, showWarnings = FALSE)
  
  # Crea la taula de freqüències relatives
  tabla <- data.frame(table(datos[, var]) / sum(table(datos[, var])))
  
  # Crea el gràfic de barres
  p <- ggplot(data = tabla, aes(x = Var1, y = Freq)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(round(Freq * 100, 2), "%")),
              vjust = 1.6, color = "white", size = 3.5) +
    theme_minimal() +
    labs(title = paste("Distribució de", var), x = var, y = "Proporció")
  
  # Desa el gràfic com a imatge dins la carpeta corresponent
  ggsave(filename = file.path(subdir, paste0("barplot_", var, ".png")), plot = p)
}


#### Per poca variabilitat decidim eliminar la variable time_signature


df_all$time_signature <- NULL
df_tr$time_signature <- NULL
df_ts$time_signature <- NULL


#----deal with NA: MICE----

library(mice)
library(dplyr)


# Exclou les columnes que no vols imputar
cols_excl <- c("global_ID", "ID","dataset","song_popularity")


# Crea una còpia del dataframe sense aquestes columnes
df_imp <- df_all[, !(names(df_all) %in% cols_excl)]

naniar::mcar_test(df_imp)

# Aplica mice sobre el subconjunt
set.seed(123)
imputed_Data <- mice::mice(df_imp, m = 5, maxit = 50, method = 'pmm', seed = 500)
mice::stripplot(imputed_Data, liveness, pch = 19, xlab = "Imputation number")


# Reincorpora les columnes ID (sense imputar)
df_all_imputed <- cbind(df_all[, cols_excl, drop = FALSE],
                       mice::complete(imputed_Data))


# check if there are still NA
colSums(is.na(df_all_imputed))

#### Gràfiques de densitat per comparar

library(ggplot2)

### Numeriques
library(ggplot2)

# Dataframes abans i després d'imputar
df_pre <- df_all
df_post <- df_all_imputed

# Selecciona només variables numèriques
varNum <- names(df_post)[sapply(df_post, is.numeric)]

# Crea la carpeta principal
dir.create("comparacions_num", showWarnings = FALSE)

for (var in varNum) {
  # Crea la subcarpeta per a cada variable
  subdir <- file.path("comparacions_num", var)
  dir.create(subdir, showWarnings = FALSE)
  
  # Construeix dataframe llarg amb dades pre i post imputació
  df_comp <- data.frame(
    valor = c(df_pre[[var]], df_post[[var]]),
    estat = rep(c("Abans imputació", "Després imputació"), each = nrow(df_pre))
  )
  
  # Crea el gràfic de densitat
  p <- ggplot(df_comp, aes(x = valor, color = estat, fill = estat)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
    scale_color_manual(values = c("#1b9e77", "#d95f02")) +
    theme_minimal() +
    labs(title = paste("Comparació pre/post imputació:", var),
         x = var, y = "Densitat", fill = "Estat", color = "Estat") +
    theme(text = element_text(size = 13))
  
  # Desa el gràfic
  ggsave(filename = file.path(subdir, paste0("comparacio_", var, ".png")), plot = p)
}


#### Categoriques
library(ggplot2)
library(reshape2)

# Selecciona variables categòriques
varCat <- names(df_post)[sapply(df_post, is.factor) | sapply(df_post, is.character)]

# Exclou les variables que no vols comparar
vars_excl <- c("global_ID", "ID", "dataset")
varCat <- setdiff(varCat, vars_excl)

# Crea la carpeta principal
dir.create("comparacions_cat", showWarnings = FALSE)

for (var in varCat) {
  subdir <- file.path("comparacions_cat", var)
  dir.create(subdir, showWarnings = FALSE)
  
  # Taules de proporcions abans i després
  pre_tab <- prop.table(table(df_pre[[var]]))
  post_tab <- prop.table(table(df_post[[var]]))
  
  # Combina en un únic dataframe (manté l’ordre de categories)
  df_comp <- data.frame(
    Categoria = names(pre_tab),
    Pre = as.numeric(pre_tab),
    Post = as.numeric(post_tab[match(names(pre_tab), names(post_tab))])
  )
  
  # Reorganitza per ggplot
  df_long <- melt(df_comp, id.vars = "Categoria",
                  variable.name = "Estat", value.name = "Proporció")
  
  # Gràfic de barres comparatiu
  p <- ggplot(df_long, aes(x = Categoria, y = Proporció, fill = Estat)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
    theme_minimal() +
    labs(title = paste("Comparació pre/post imputació:", var),
         x = var, y = "Proporció", fill = "Estat") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          text = element_text(size = 13))
  
  # Desa el gràfic
  ggsave(filename = file.path(subdir, paste0("comparacio_", var, ".png")), plot = p)
}


##### Descriptiva univariant post imputacio
# --- SELECCIÓ DE VARIABLES ---
varNum <- names(df_all_imputed)[sapply(df_all_imputed, is.numeric)]
varCat <- names(df_all_imputed)[sapply(df_all_imputed, is.factor) | sapply(df_all_imputed, is.character)]

# Exclou identificadors
vars_excl <- c("global_ID", "ID", "dataset")
varNum <- setdiff(varNum, vars_excl)
varCat <- setdiff(varCat, vars_excl)

# =========================================================
# 🔹 NUMÈRIQUES
# =========================================================
library(ggplot2)

# --- SELECCIÓ DE VARIABLES NUMÈRIQUES ---
varNum <- names(df_all_imputed)[sapply(df_all_imputed, is.numeric)]
vars_excl <- c("global_ID", "ID", "dataset")
varNum <- setdiff(varNum, vars_excl)

# --- CREACIÓ DE LA CARPETA PRINCIPAL ---
dir.create("plots_num_df_all_imputed", showWarnings = FALSE)

# --- BUCLE PER CREAR ELS GRÀFICS ---
for (var in varNum) {
  
  # Subcarpeta per cada variable
  subdir <- file.path("plots_num_df_all_imputed", var)
  dir.create(subdir, showWarnings = FALSE)
  
  # --- HISTOGRAMA AMB DENSITAT ---
  histo <- ggplot(df_all_imputed, aes(x = .data[[var]])) + 
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 30) +
    geom_density(alpha = 0.2, fill = "#FF6666") +
    geom_vline(aes(xintercept = mean(.data[[var]], na.rm = TRUE)),
               color = "blue", linetype = "dashed", linewidth = 1) +
    ggtitle(paste("Histograma i densitat de", var)) +
    theme_minimal() +
    theme(text = element_text(size = 13))
  
  # --- BOXPLOT ---
  boxp <- ggplot(df_all_imputed, aes(x = .data[[var]])) + 
    geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4) +
    ggtitle(paste("Boxplot de", var)) +
    theme_minimal() +
    theme(text = element_text(size = 13))
  
  # --- DESA ELS GRÀFICS ---
  ggsave(filename = file.path(subdir, paste0("histograma_", var, ".png")), plot = histo)
  ggsave(filename = file.path(subdir, paste0("boxplot_", var, ".png")), plot = boxp)
}


# =========================================================
# 🔹 CATEGÒRIQUES
# =========================================================
dir.create("comparacions_cat_df_all_imputed", showWarnings = FALSE)

for (var in varCat) {
  subdir <- file.path("comparacions_cat_df_all_imputed", var)
  dir.create(subdir, showWarnings = FALSE)
  
  tab <- prop.table(table(df_all_imputed[[var]], useNA = "ifany"))
  df_tab <- data.frame(Categoria = names(tab), Proporcio = as.numeric(tab))
  
  p <- ggplot(df_tab, aes(x = Categoria, y = Proporcio)) +
    geom_bar(stat = "identity", fill = "#7570b3") +
    theme_minimal() +
    labs(title = paste("Distribució de", var, "(df_all_imputed)"),
         x = var, y = "Proporció") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          text = element_text(size = 13))
  
  ggsave(filename = file.path(subdir, paste0("distribucio_", var, ".png")), plot = p)
}


### Descriptiva bivariant post imputacio
dadesimp<-df_all_imputed

## Numeriques vs numeriques

# Carregar llibreries necessàries
library(ggplot2)

# Crear la carpeta si no existeix
dir.create("num_num", showWarnings = FALSE)

# Seleccionar només les variables numèriques
num_dades <- dadesimp[sapply(dadesimp, is.numeric)]

# Obtenir totes les combinacions possibles de variables numèriques
comb <- combn(names(num_dades), 2, simplify = FALSE)

# Generar scatterplots i guardar la correlació en un document
for (par in comb) {
  var1 <- par[1]
  var2 <- par[2]
  
  # Calcular el coeficient de correlació
  corr_value <- cor(num_dades[[var1]], num_dades[[var2]], use = "complete.obs")
  
  # Crear el gràfic
  p <- ggplot(dadesimp, aes_string(x = var1, y = var2)) +
    geom_point(alpha = 0.6, color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = paste("Scatterplot de", var1, "vs", var2),
         subtitle = paste("Correlació:", round(corr_value, 2)),
         x = var1, y = var2) +
    theme_minimal(base_size = 14)
  
  # Guardar el gràfic a la carpeta
  ggsave(filename = paste0("num_num/", var1, "_vs_", var2, ".png"),
         plot = p, width = 7, height = 5, dpi = 300)
  
  # Guardar la correlació en un document .txt
  writeLines(
    paste("Coeficient de correlació entre", var1, "i", var2, ":", round(corr_value, 4)),
    paste0("num_num/", var1, "_vs_", var2, ".txt")
  )
}


### Numeriques vs categoriques
library(ggplot2)
library(dplyr)

# Crear la carpeta si no existeix
dir.create("num_cat", showWarnings = FALSE)

# Seleccionar variables numèriques i categòriques (factor)
num_dades <- dadesimp[sapply(dadesimp, is.numeric)]
cat_dades <- dadesimp[sapply(dadesimp, is.factor)]
cat_dades$ID<-NULL


# Creuar totes les variables numèriques amb categòriques
for (num_var in names(num_dades)) {
  for (cat_var in names(cat_dades)) {
    
    # Crear el resum estadístic de la numèrica per cada nivell de la categòrica
    resum <- dadesimp %>%
      group_by(.data[[cat_var]]) %>%
      summarise(
        Mínim = min(.data[[num_var]], na.rm = TRUE),
        Q1 = quantile(.data[[num_var]], 0.25, na.rm = TRUE),
        Mediana = median(.data[[num_var]], na.rm = TRUE),
        Mitjana = mean(.data[[num_var]], na.rm = TRUE),
        Q3 = quantile(.data[[num_var]], 0.75, na.rm = TRUE),
        Màxim = max(.data[[num_var]], na.rm = TRUE),
        Desviació = sd(.data[[num_var]], na.rm = TRUE),
        N = sum(!is.na(.data[[num_var]]))
      )
    
    # Guardar el resum en un fitxer .txt
    write.table(
      resum, file = paste0("num_cat/", num_var, "_vs_", cat_var, ".txt"),
      sep = "\t", row.names = FALSE, quote = FALSE
    )
    
    # Crear el boxplot
    p1 <- ggplot(dadesimp, aes_string(x = cat_var, y = num_var)) +
      geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
      labs(title = paste("Boxplot de", num_var, "per", cat_var)) +
      theme_minimal(base_size = 14)
    
    # Guardar el boxplot
    ggsave(filename = paste0("num_cat/", num_var, "_vs_", cat_var, "_boxplot.png"),
           plot = p1, width = 7, height = 5, dpi = 300)
    
    # Crear l'histograma en valors relatius
    p2 <- ggplot(dadesimp, aes_string(x = num_var, y = "..density..", fill = cat_var)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      labs(title = paste("Histograma de", num_var, "segons", cat_var),
           y = "Densitat (freqüència relativa)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    # Guardar l'histograma
    ggsave(filename = paste0("num_cat/", num_var, "_vs_", cat_var, "_histograma.png"),
           plot = p2, width = 7, height = 5, dpi = 300)
  }
}


### Categòrica vs categòrica
# Carregar llibreries necessàries
library(ggplot2)
library(dplyr)

# Crear la carpeta si no existeix
dir.create("cat_cat", showWarnings = FALSE)

# Seleccionar només les variables categòriques (factor)
cat_dades <- dadesimp[sapply(dadesimp, is.factor)]
cat_dades$ID<-NULL

# Creuar totes les variables categòriques entre elles
for (cat_var1 in names(cat_dades)) {
  for (cat_var2 in names(cat_dades)) {
    
    # Evitar que es faci la combinació de la mateixa variable amb ella mateixa
    if (cat_var1 == cat_var2) next
    
    # Crear la taula de contingència
    taula_contingencia <- table(dadesimp[[cat_var1]], dadesimp[[cat_var2]])
    
    # Calcular els pesos relatius
    taula_contingencia_relativa <- prop.table(taula_contingencia)
    
    # Calcular percentatges per fila i per columna
    percentatge_fila <- prop.table(taula_contingencia, margin = 1) * 100
    percentatge_columna <- prop.table(taula_contingencia, margin = 2) * 100
    
    # Guardar la taula de contingència, pesos relatius i percentatges en un fitxer .txt
    write.table(
      cbind(taula_contingencia, "Pesos Relatius" = round(taula_contingencia_relativa, 4),
            "Percentatge Fila" = round(percentatge_fila, 2),
            "Percentatge Columna" = round(percentatge_columna, 2)),
      file = paste0("cat_cat/", cat_var1, "_vs_", cat_var2, "_taula_contingencia.txt"),
      sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE
    )
    
    # Realitzar el test d'independència Chi quadrat
    test_chi <- chisq.test(taula_contingencia)
    
    # Guardar els resultats del test Chi quadrat en un fitxer .txt
    writeLines(
      paste("Test d'Independència Chi Quadrada per", cat_var1, "i", cat_var2, ":\n",
            "Estadístic X² =", round(test_chi$statistic, 2), "\n",
            "P-value =", round(test_chi$p.value, 4), "\n",
            "Graus de llibertat =", test_chi$parameter),
      paste0("cat_cat/", cat_var1, "_vs_", cat_var2, "_test_chi.txt")
    )
    
    # Crear el gràfic de barres múltiple
    p <- ggplot(dadesimp, aes_string(x = cat_var1, fill = cat_var2)) +
      geom_bar(position = "fill", alpha = 0.7) +
      labs(title = paste("Gràfic de barres de", cat_var1, "vs", cat_var2),
           y = "Proporció", x = cat_var1) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    # Guardar el gràfic de barres múltiple
    ggsave(filename = paste0("cat_cat/", cat_var1, "_vs_", cat_var2, "_barres.png"),
           plot = p, width = 7, height = 5, dpi = 300)
  }
}




