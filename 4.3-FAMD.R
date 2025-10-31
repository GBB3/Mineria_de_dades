#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/

# He de mirar les cordenades dels individus. No importa ni saber que significa cada dim sino nomes les cordenades dels individus. El que mirem es tenir el mateix numero d'observacions pero potden canviar el numero de vcariables
#Hem d'explicar un minim del 90%
## Import libraries
library(FactoMineR)
library(factoextra)

## Import data
load("df_outliers.RData")
df=df_outliers

## Preview data
head(df)

df=df[,-c(1,2,3,4)]# eliminim variable resposta

key_mapping <- c(
  "0" = "C",
  "1" = "C#",
  "2" = "D", 
  "3" = "D#",
  "4" = "E",
  "5" = "F",
  "6" = "F#",
  "7" = "G",
  "8" = "G#",
  "9" = "A",
  "10" = "A#",
  "11" = "B"
)

# fem els canvis seguents a les variables categòriques per
# no repetir identificador i que peti FAMD


df$key <- key_mapping[as.factor(df$key)]
df$key=as.factor(df$key)
df$is_outlier <- ifelse(df$is_outlier == 1, "Si", "No")
df$is_outlier=as.factor(df$is_outlier)
str(df)


res.famd <- FAMD(df, graph = FALSE, ncp=Inf)
print(res.famd)

summary(res.famd)# tenim que amb 19 dimensions pasem del 90% de var explicada i el 100% a la 23

ind <- get_famd_ind(res.famd)

datamod=ind$coord

nrow(datamod)# comprobem que tenim les mateixes files.
