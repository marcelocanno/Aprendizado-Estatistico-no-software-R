

# analise de agrupamento

# bano de dados do R
data(iris)

# apresenta os seis primeiros linhas do conjunto de dados
head(iris)

# Recebe dados do iris
dados <- iris

# calcula da distancia
D <- dist(dados[,-5], method = "euclidean")^2

# agrupamento hierarquico
agr <- hclust(D, method = "complete")

# dendograma
plot(agr, xlab = "", ylab = "Distancia Eucladiana Quadratica")

plot(agr, xlab = "", ylab = "distancia Eucladiana Quadratica", cex=0.3)


# correlacao cofernetica
cof <- cophenetic(agr)
cor(cof, D)


#numero de grupos
grupos <- 3
rect.hclust(agr,k=grupos)

# Apresentacao dos grupos
grupos <- cutree(agr, k=grupos)
grupos



# K-medias
km <- kmeans(x=iris[,-5], 3, nstart = 20)
km

# Apresentacao dos grupos
cluster <- km$cluster
cluster

