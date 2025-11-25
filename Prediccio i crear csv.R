rferr <- read.csv("rflog1.csv")

# Calcular quants valors representen el 98%
n <- nrow(rferr)
cutoff_n <- floor(n * 0.97)

# Ordenar els índexs segons song_popularity (de menor a major)
idx_sorted <- order(rferr$song_popularity)

# Agafar el 98% més baix
idx_low98 <- idx_sorted[1:cutoff_n]

# Posar aquests valors a 0
rferr$song_popularity[idx_low98] <- 0

# Exportar a CSV
write.csv(rferr, "97log1.csv", row.names = FALSE)
