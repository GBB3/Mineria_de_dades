# Calcular IQR només amb les files de train
Q1 <- quantile(df_no_outliers_extrems_no_instrumetalness$instrumentalness[
  df_no_outliers_extrems_no_instrumetalness$dataset == "train"
], 0.25)

Q3 <- quantile(df_no_outliers_extrems_no_instrumetalness$instrumentalness[
  df_no_outliers_extrems_no_instrumetalness$dataset == "train"
], 0.75)

IQR_value <- Q3 - Q1

lower_bound <- Q1 - 10 * IQR_value
upper_bound <- Q3 + 10 * IQR_value


df_filtered <- df_no_outliers_extrems_no_instrumetalness |>
  filter(
    (dataset != "train") |
      (dataset == "train" & instrumentalness >= lower_bound & instrumentalness <= upper_bound)
  )

df_all_transformed <- df_filtered |>
  mutate(
    liveness = log1p(liveness),
    song_duration_ms = log1p(song_duration_ms),
    acousticness = log1p(acousticness),
    speechiness = log1p(speechiness),
    instrumentalness = log1p(instrumentalness),
    tempo = scale(tempo),
    key = as.factor(key),
    audio_mode = as.factor(audio_mode)
  )


df_all_transformed <- df_no_outliers_extrems_no_instrumetalness|> 
  dplyr::mutate(
    # Transformacions logarítmiques o arrel
    liveness = log1p(liveness),
    song_duration_ms = log1p(song_duration_ms),
    acousticness = log1p(acousticness),
    speechiness = log1p(speechiness),
    instrumentalness = log1p(instrumentalness),
    
    # Escalat de tempo
    tempo = scale(tempo),
    
    # Variables categòriques
    key = as.factor(key),
    audio_mode = as.factor(audio_mode)
  )
df_all_transformed$key<-NULL
df_all_transformed$audio_mode<-NULL


save(df_all_transformed, file = "df_all_transformedsensekeyaudioNOTEMPO.RData")

