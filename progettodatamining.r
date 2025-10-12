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
df_ts$song_popularity <- 0

# reorder of col names of test so they are same order of train
df_ts <- df_ts[, names(df_tr)]

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


#----deal with NA: MICE----

library(mice)
library(dplyr)

# Remove cols i don't have to impute
cols_to_impute <- setdiff(names(df_all), c("ID", "global_ID", "dataset"))

# Select subset
df_mice <- df_all[, cols_to_impute]

# MICE  
set.seed(123)
imp <- mice(df_mice, method = "pmm", m = 1, maxit = 10, printFlag = TRUE)
df_imputed <- complete(imp)


#----variables check----

df_all_imputed <- df_all
df_all_imputed[, cols_to_impute] <- df_imputed

# duplicate and convert
df_all_imputed <- df_all_imputed %>%
  mutate(
    key_num = key,
    time_signature_num = time_signature,
    mode_num = audio_mode,
    key_cat = as.factor(round(key)),  
    time_signature_cat = as.factor(round(time_signature)),
    mode_cat = as.factor(round(audio_mode))
  ) %>%
  select(-c(key, audio_mode, time_signature))  # remove originals

# check if there are still NA
colSums(is.na(df_all_imputed))

#----check for unbalanced----

# Check the distribution of key_cat 
summary(df_all_imputed$key_cat)

# The 'key' represents the musical key (0–11), corresponding to the 12 semitones.
# A balanced distribution across values is normal since songs appear in many different keys.

# --- Interpretation ---
# ✅ All 12 musical keys (0 to 11) are present.
# ✅ Frequencies are reasonably balanced — no category dominates excessively.
# This indicates that the MICE imputation maintained a realistic distribution.

# --- Optional: visualize the distribution of musical keys ---
# This helps to confirm visually that MICE did not distort the key variable.
library(ggplot2)

ggplot(df_all_imputed, aes(x = key_cat)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Distribution of Musical Keys (key_cat)",
    x = "Musical Key (0–11)",
    y = "Number of Songs"
  )


# check distribution of time_signature_cat
summary(df_all_imputed$time_signature_cat)

table(df_tr$time_signature, useNA = "ifany")

# In music terms, there’s no real "0/4" or "1/4" meter — so these are either data-entry errors or placeholders!!!

# solution: consider 0 and 1 as 4:It’s musically valid, statistically safe, and keeps your dataset consistent for modeling.
# after that do MICE and then treat as categorical variable!


#----correct overflow----

# fix invalid values first
df_all$time_signature[df_all$time_signature %in% c(0, 1)] <- 4

# run MICE
library(mice)
set.seed(123)
imp <- mice(df_all[, -c(1)], method = "pmm", m = 1, maxit = 10)
df_all_imputed <- complete(imp)

# After imputation, create categorical versions
library(dplyr)

df_all_imputed <- df_all_imputed %>%
  mutate(
    key_num = key,
    time_signature_num = time_signature,
    mode_num = audio_mode,
    key_cat = as.factor(round(key)),
    time_signature_cat = as.factor(round(time_signature)),
    mode_cat = as.factor(round(audio_mode))
  ) %>%
  select(-c(key, audio_mode, time_signature))

# check result
summary(df_all_imputed$time_signature_cat)   #is unbalanced: decide what to do!
summary(df_all_imputed$key_cat)   #seems ok
summary(df_all_imputed$mode_cat)   #seems ok


# check if there are still NA
colSums(is.na(df_all_imputed))


#----does time signature influents popularity?----
library(ggplot2)

ggplot(df_all_imputed %>% filter(dataset == "train"),
       aes(x = time_signature_cat, y = song_popularity)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Song Popularity by Time Signature",
    x = "Time Signature",
    y = "Popularity"
  )

# There’s no meaningful difference between categories. No category seems to have systematically higher or lower popularity.

# possible solution: It’s safe (and probably best) to drop time_signature_cat from the model — it won’t hurt performance, and you’ll have a cleaner feature set.

#----numerical variables distribution----

library(ggplot2)

num_vars <- c("danceability", "energy", "loudness", "speechiness",
              "acousticness", "instrumentalness", "liveness", 
              "audio_valence", "tempo", "song_duration_ms", 
              "key_num","time_signature_num", "mode_num")

# Create a histogram for each numerical variable.
for (var in num_vars) {
  p <- ggplot(df_all_imputed, aes(x = .data[[var]])) +
    geom_histogram(bins = 30, fill = "skyblue", color = "white") +
    labs(title = paste("Distribution of", var),
         x = var,
         y = "Frequence") +
    theme_minimal()
  print(p)
}

# ---- The variable instrumentalness seems to have very low variability (almost all values are 0), let's check -----
# 1. Variance
var(df_all_imputed$instrumentalness)

# 2. % of 0 and values over 0.5
mean(df_all_imputed$instrumentalness == 0) * 100
mean(df_all_imputed$instrumentalness > 0.5) * 100

#correlation with song_popularity
cor(df_all_imputed$instrumentalness, df_all_imputed$song_popularity, use = "complete.obs")

ggplot(df_all_imputed, aes(x = instrumentalness, y = song_popularity)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Relation between instrumentalness and song_popularity",
       x = "Instrumentalness", y = "Song Popularity")

summary(lm(song_popularity ~ instrumentalness, data = df_all_imputed))

# ---- correlation of all numerical variables with song_popularity ----

num_vars <- c("danceability", "energy", "loudness", "speechiness",
              "acousticness", "instrumentalness", "liveness", 
              "audio_valence", "tempo", "song_duration_ms", 
              "key_num", "time_signature_num", "mode_num")

# Calculation of correlations with song_popularity.
correlations <- sapply(num_vars, function(var) {
  cor(df_all_imputed[[var]], df_all_imputed$song_popularity, use = "complete.obs")
})

# Convert to a data frame for clearer visualization.
correlations_df <- data.frame(
  Variable = num_vars,
  Correlation = round(correlations, 3)
)

# Sort by the absolute value of the correlation.
correlations_df <- correlations_df[order(abs(correlations_df$Correlation), decreasing = TRUE), ]

print(correlations_df)

# ---- correlation between variables ----
var <- c("danceability", "energy", "loudness", "speechiness",
  "acousticness", "instrumentalness", "liveness", 
  "audio_valence", "tempo", "song_duration_ms", "key_num")

# Compute the correlation matrix only for the numerical variables.
cor_matrix <- cor(df_all_imputed[, var])

# Transform the matrix into a long format (pairs of variables).
cor_pairs <- as.data.frame(as.table(cor_matrix))

# Filter only the strong correlations (> 0.5 or < -0.5).
high_corr <- subset(cor_pairs, abs(Freq) > 0.5)

# Sort by the strength of the correlation.
high_corr <- high_corr[order(-abs(high_corr$Freq)), ]

# Display the pairs.
print(high_corr)

# energy, acousticness and loudness are very correlated between each other. We should not use them together.

# let's try some models
model <- lm(song_popularity ~ danceability + energy + loudness + speechiness +
              acousticness + instrumentalness + liveness + audio_valence +
              tempo + key_num + time_signature_num + mode_num,
            data = df_all_imputed)

summary(model)

model1 <- lm(song_popularity ~ danceability + energy + loudness +
              acousticness + instrumentalness + liveness + audio_valence +
              tempo + song_duration_ms + key_num,
            data = df_all_imputed)

summary(model1)

model2 <- lm(song_popularity ~ danceability + energy + instrumentalness + liveness + audio_valence +
               tempo + song_duration_ms + key_num,
             data = df_all_imputed)

summary(model2)

model3 <- lm(song_popularity ~ danceability + energy + loudness +
               acousticness + instrumentalness + liveness + audio_valence +
               tempo + key_num,
             data = df_all_imputed)

summary(model3)

# not seems to have a logic. maybe we should detect outliers.








