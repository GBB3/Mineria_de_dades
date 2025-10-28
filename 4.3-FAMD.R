## Import libraries
library(FactoMineR)
library(factoextra)

## Import data
load("df_outliers.RData")
df=df_outliers

## Preview data
head(df)

df=df[,-4]# eliminim variable resposta

## FAMD
res.famd <- FAMD(df, graph = FALSE)
print(res.famd)

## Eigenvalues
eig.val <- get_eigenvalue(res.famd)
head(eig.val)

## Scree Plot
fviz_eig(res.famd, addlabels = TRUE)

#Colze en 3dim

## Totes les variables
(var <- get_famd_var(res.famd))
head(var$coord[, 1:4])
head(var$cos2[, 1:4])
head(var$contrib[, 1:4])

##Correlacio entre TOTES les variables
fviz_famd_var(res.famd, repel = TRUE, axes = c(1,2))
fviz_famd_var(res.famd, repel = TRUE, axes = c(1,3))
fviz_famd_var(res.famd, repel = TRUE, axes = c(2,3))

## Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
## Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)
## Contribution to the third dimension
fviz_contrib(res.famd, "var", axes = 3)

## Variables Numeriques
(quanti.var <- get_famd_var(res.famd, "quanti.var"))
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black", axes = c(1,2))
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black", axes = c(1,3))
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black", axes = c(2,3))
#cor
corrplot(res.famd$quanti.var$cos2, is.corr = FALSE)

#contrib
fviz_contrib(res.famd, choice = "var", axes = 1:3)
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes = c(1,2))
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes = c(1,3))
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes = c(2,3))

#cos2
fviz_cos2(res.famd, choice = "var", axes = 1:3)
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes = c(1,2))
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes = c(1,3))
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, axes = c(2,3))

## Variables Qualitatives
(quali.var <- get_famd_var(res.famd, "quali.var"))

#contrib
fviz_contrib(res.famd, choice="var", axes=1:3)
fviz_mca_var(res.famd, col.var="contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE, ggtheme=theme_minimal(),axes = c(1,2))
fviz_mca_var(res.famd, col.var="contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE, ggtheme=theme_minimal(),axes = c(1,3))
fviz_mca_var(res.famd, col.var="contrib", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE, ggtheme=theme_minimal(),axes = c(2,3))
#una mica repetitiu amb el que ja hem vist

fviz_famd_var(res.famd, "quali.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(1,2))
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(1,3))
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(2,3))
#mes interesant

#cos2
fviz_contrib(res.famd, choice="var", axes=1:3)
fviz_mca_var(res.famd, col.var="cos2", gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE, ggtheme=theme_minimal())

fviz_famd_var(res.famd, "quali.var", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(1,2))
fviz_famd_var(res.famd, "quali.var", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(1,3))
fviz_famd_var(res.famd, "quali.var", col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),axes = c(2,3))


## Individus
(ind <- get_famd_ind(res.famd))

#cont ind
fviz_contrib(res.famd, choice="ind", axes=1:3, top=15)
fviz_contrib(res.famd, choice="ind", axes=1:3)
#elipses de les variables més representades
fviz_ellipses(res.famd, c("Order.Priority", "Ship.Mode","Category","Sub.Category"), geom="point", axes=c(1,2))
fviz_ellipses(res.famd, c("Order.Priority", "Ship.Mode","Category","Sub.Category"), geom="point", axes=c(1,3))
fviz_ellipses(res.famd, c("Order.Priority", "Ship.Mode","Category","Sub.Category"), geom="point", axes=c(2,3))

#Representacio dels individus per cos2 (triga moltissim)
fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

