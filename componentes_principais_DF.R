
# Exercicio aula 3 pr√°tica
setwd("C:/Users/canno/OneDrive/Documentos/")
dados <- read.csv("componentes_principais.csv", header=T)
dados

# ObtenÁ„o da matriz de covariancia
S <- var(dados[,-1])
S

# ObtenÁ„o dos autovalores e autovetores
autos <- eigen(S)
autos

# Escores
dad <- as.matrix(dados[,-1])
escores1 <- dad%*%autos$vectors
escores1

# PERCENTUAL DE EXPLICA«√O
percent_explic <- autos$values/sum(diag(S))
percent_explic


id <- dados[,1]
cp_plot <- cbind(id,escores1)
plot(cp_plot[,2], cp_plot[,3], type = "n", xlab = "Componente principal 1", ylab =  "Componentes principal 2")
text(y=cp_plot[,3], x=cp_plot[,2])
# Segunda sess„o da primeira aula


data(iris)
#data
# Apresenta seis primeiros dados
head(iris)

# Apresenta os ultimos valores
tail(iris)

# covariancia
pca_Cov <- princomp(iris[,-5], cor = FALSE, scores = TRUE)
summary(pca_Cov)

# pesos
pca_Cov$loadings

# obter escores seis primeiros escores
head(pca_Cov$scores)

# Analise grafica numero linhas e janelas graficas
par(mfrow=c(1,2))
biplot(pca_Cov)

# obtenÁ„o corelaÁ„o
pca_Cor <- princomp(iris[,-5], cor = TRUE, scores = TRUE)
summary(pca_Cor)


# plotando dendograma
biplot(pca_Cor)

#dados<- iris
variancias <- apply(iris[,-5], 2, var)
variancias




