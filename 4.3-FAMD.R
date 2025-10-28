#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/
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
