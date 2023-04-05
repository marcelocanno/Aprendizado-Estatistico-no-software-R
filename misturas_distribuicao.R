
#Instalando pacote
#install.packages("mclust")

library(mclust)
data(iris)
head(iris)

# realizar o ajuste do modelo

mBIC = mclustBIC(iris[-5])
head(mBIC)
summary(mBIC)
plot(mBIC)

# Ajustar e obter as estimativas dos parametros dos melhores modelos

mBIC1 = Mclust(iris[-5], x = mBIC)
summary(mBIC1, parameters = TRUE)

#Plote classificar

plot(mBIC1, what = "classification")

#Plot intenso

plot(mBIC1, what = "uncertainty")

a <- uncerPlot(z = mBIC1$z)
a1 <- uncerPlot(z = mBIC1$z, verdade = iris[,5])


table(iris$Species, mBIC1$classification)

# considerando o modelo VEV, 3

mBIC3 = Mclust(iris[-5], G = 3, modelNames = "VEV")
summary(mBIC3, parameters = TRUE)

# plot classificacao

plot(mBIC3, what = "classification")

# plot Incerteza

plot(mBIC3, what = "uncertainty")


# tabela especie classificacao

table(iris$Species, mBIC3$classification)


# Considerando o modelo VEV, 3

mBIC3 = Mclust(iris[-5], G = 3,modelNames = "VEV")
summary(mBIC3, parametros = TRUE)

# Plote classificar

plot(mBIC3, what = "classification")

#Plot intenso
plot(mBIC3, what = "uncertainty")

table(iris$Species, mBIC3$classification)
