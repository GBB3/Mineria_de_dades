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
