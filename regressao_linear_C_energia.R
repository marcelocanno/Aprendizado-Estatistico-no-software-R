
# aula 4 - pratica - Regressao linear simples e multipla.

# Definir o caminho dos dados
setwd("C:/Users/canno/Downloads/")
dados <- read.csv("consuumo_energia.csv.csv", header = T)

# Exolorar conjunto de dados

names(dados)
head(dados)
str(dados)
# Resumo dos dados

summary(dados)

# Correlogramas entre pares de variaveis

pairs(dados, col = 2 , pch = 19)

mod1 <- lm(consumo ~ temperatura + dias + pureza + producao, data = dados)
summary(mod1)

# Modelo de regressao simples

mod2 <- lm(consumo ~ temperatura, data=dados)
summary(mod2)


# Predizer o consumo medio para uma temperatura de 51

novo <- data.frame(temperatura = 51)
predict(mod2, novo)


