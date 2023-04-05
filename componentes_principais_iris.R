#Laboratório Componentes Principais

#Banco de dado Iris

#Obter informações sobre o conjunto de dados iris

data(iris)


#Apresentar as seis primeiras linhas do conjunto de dados

tail(iris)
head(iris)
#Verificar dimensão do conjunto de dados
dim(iris)




#Componentes principais

pca_Cov <- princomp (iris[, -5], cor = FALSE, scores = TRUE)
pca_Cov
summary(pca_Cov)


pca_Cov$loadings

head(pca_Cov$scores)

par(mfrow = c(1,2))
biplot(pca_Cov)


pca_Cor <- princomp(iris[,-5], cor = TRUE, scores = TRUE)
summary(pca_Cor)

biplot(pca_Cor)

variancias <- apply(iris[,-5], 2, var)
variancias


par(mfrow=c(1,2),cex=0.7)
biplot(pca_Cov)
abline(a=0,b=0)
abline(h=0,v=0)
biplot(pca_Cor)
abline(a=0,b=0)
abline(h=0,v=0)

