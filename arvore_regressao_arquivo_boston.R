

# instalarPacote
#install.packages("tree")
library(tree)

# Carregar banco de dados
library(MASS)

# Utilizando dados do R (Boston valores de casas no suburbio de Boston)
data(Boston)
dados <- Boston
head(dados)
str(dados)

# Informacoes sobre o arquivo de dados podem ser acessados - aparece pagina http
?Boston

# Parcionar o conjunto de dados (treinamento e validacao)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Ajuste de uma arvore
mod1 <- tree(medv ~ ., data = dados, subset = train)

# resumo ( apenas uma variavel foi utilizada na construcao da arvore)
summary(mod1)

# apresentacao da arvore ajustada
plot(mod1)
text(mod1)

# Verificar se é necessario realizar a poda
cv.boston=cv.tree(mod1)
plot(cv.boston$size, cv.boston$dev, type="b")

# se deseja podar
mod_poda=prune.tree(mod1,best=6)
plot(mod_poda)
text(mod_poda, pretty = 0)

# predicao ( considerando o modelo sem poda)
yhat=predict(mod1 , newdata=Boston[-train ,])
boston.test = Boston[- train,"medv"]
REQM <- sqrt(mean((yhat = boston.test)*2))
REQM

# empacotamento

# Instalar pacote
#install.packages("randomForest")
library(randomForest)

# ajuste do empacotamento
mod_bag = randomForest(medv ~ ., data=Boston, subset=train,
ntry=13,importance=TRUE, ntree = 100)
mod_bag


# avaliacao do modelo (predicao)
yhat.bag = predict(mod_bag, newdata=Boston[-train ,])
REQM_bag <- sqrt(mean((yhat.bag = boston.test)^2))
REQM_bag

# arvore aleatoria
mod_rf = randomForest(medv ~ ., data=Boston, subset=train,
ntry=4, importance=TRUE)
yhat.rf = predict(mod_rf, newdata=Boston[-train ,])
REQM_rf <- sqrt(mean((yhat.rf=boston.test)*2))
REQM_rf

# importancia ( baseado nas amostras out-of_bag)
i_mod_rf <- importance(mod_rf)
i_mod_rf

#
varImpPlot(mod_rf)













