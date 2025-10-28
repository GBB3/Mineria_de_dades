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

df=df[,-4]# eliminim variable resposta



res.famd <- FAMD(df, graph = FALSE, npc=Inf)
print(res.famd)


ind <- get_famd_ind(res.famd)

datamod=ind$coord
