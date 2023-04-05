
# Instalar Pacote
# install.packages("tree")

library(tree)

# Utilizando dados do R (Iris)
# Leitura dos dados

data(iris)
dados <- iris
head(dados)
str(iris)

# Particionar o conjunto de dados (Treinamento e validacao)

iris_setosa<-iris[iris$Species=="setosa",]
iris_versicolor <- iris[iris$Species=="versicolor",]
iris_virginica <- iris[iris$Species=="virginica",]
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])


# Ajuste de uma arvore

arvore1 <- tree(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width, data = iris_train)

# Resumo (Apenas uma variavel foi utilizada na construcao da arvore)

summary(arvore1)

# Apresentacao da arvore ajustada

plot(arvore1)
text(arvore1)

# Verificar se e interessante realizar a poda

cv.iris=cv.tree(arvore1)
plot(cv.iris$size ,cv.iris$dev ,type="b")

# Se desejar podar

mod_poda=prune.tree(arvore1,best=3)
plot(mod_poda)
text(mod_poda, pretty =0)

# Predissao (Considerando o modelo sem poda)

pred_arv <- predict(mod_poda, iris_test, type="class")
head(pred_arv)

# Medidas de performance (https://topepo.github.io/caret/variable-importance.html)
# install.packages("caret")
library (caret)
confusionMatrix(pred_arv, iris_test$Species)

# bagging (ensacamento)

# PacoteInstalar
# install.packages("randomForest")
library(randomForest)

# bagging
# Ajuste (bagging)
bagging <- randomForest(Species~., data=iris_train, mtry = 4)


# Avaliacao do modelo (Predicao)
pred_bagg <- predict(bagging, iris_test)
confusionMatrix(pred_bagg, iris_test$Species)

# Floresta Aleatoria
rf<- randomForest(Species~., data=iris_train, mtry = 2)
rf

# Avaliacao do modelo (Predicao)
pred_rf <- predict(rf, iris_test)
confusionMatrix(pred_rf, iris_test$Species)

# Importanncia (Baseado nas amostras out-of_bag)
i_mod_rf <-importance(rf)
i_mod_rf

varImpPlot (rf)
