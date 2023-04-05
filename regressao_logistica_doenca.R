
# Definisao de caminho

setwd("C:/Users/canno/OneDrive/Documentos/")

# leitura dos dados
dados <- read.csv("funcao_distribuicao.csv", h=T)
head(dados)

# visualizacao dos dados
View(dados)
str(dados)
dados
summary(dados)

# ajuste do modelo
reg <- lm(Doenca ~ tm + ur, data=dados)
summary(reg)

# Probabilidade do ano ter a doenca
reg$fitted

# Figura ( dados em funcao das probabilidades)
plot(dados$Ano,reg$fitted, xlab="ano",
ylab = "Probabilidade estimada de ter a doenca", ylim = c(-0.2,1.2))

# Adicionando espaco parametrico de probabilidade
abline(h=0, col="red")
abline(h=1, col="red")

# Acessando medidas de qualidade
prob <- reg$fitted.values
prob

# Classificacao em classes
reg.classe <- ifelse(prob > 0.5, 1, 0)
reg.classe

# Tabela de confusao
tabela <- table(dados$Doenca, reg.classe)
tabela

# taxa de erro aparente
TEA <- 1 - sum(diag(tabela))/sum(tabela)
TEA

# Acuracia
AC <- sum(diag(tabela))/sum(tabela)
AC

# Sensibilidade (verdadeiros positivos)
S <- tabela[2,2]/ sum(tabela[2,])
S

# Especificidade (verdadeiro negativos)
E <- tabela[1,1]/ sum(tabela[1,])
E

# modelo Logit (Nao lineariadade entre T e variaveis explicitas)
logit <- glm(Doenca ~ tm + ur , data = dados, family = binomial(link = "logit"))

# Valores de probabilidade estimados
prob <- logit$fitted.values
prob

# Classificacao
prob <- logit.classe <- ifelse(prob > 0.5, 1, 0)
prob

# Acessando medidas de qualidade
# Tabela de confusao
tabela <- table(dados$Doenca, logit.classe)
tabela

# Taxa do erro aparente
TEA <- 1 - sum(diag(tabela))/sum(tabela)
TEA


# Acuracia
AC <- sum(diag(tabela))/sum(tabela)
AC

# Sensibilidade (verdadeiros positivos)
S <- tabela[2,2]/ sum(tabela[2,])
S

# Especificidade (verdadeiro negativos)
E <- tabela[1,1]/ sum(tabela[1,])
E


# logit multinomial ( mais de duas classes)
# instalar pacote
#install.packages("nnet")
library(nnet)

# Leitura de dados
dados <- iris
head(dados)

# ajuste
logitm <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = dados)

# Probabilidade estimadas
prob_logitm <- predict(logitm, dados[,1:4], "probs")
prob_logitm

# Classes
Classe <- apply(prob_logitm, 1, which.max)
head(Classe)

# Codificacao original
Classe[which(Classe=="1")] <- levels(dados$Species)[1]
Classe[which(Classe=="2")] <- levels(dados$Species)[2]
Classe[which(Classe=="3")] <- levels(dados$Species)[3]
head(Classe)

# Acessando medidas de qualidade
# tabela de confusao
tabela1 <- table(dados$Species, Classe)
tabela1


# taxa de erro aparente
TEA <- 1 - sum(diag(tabela1))/ sum(tabela1)
TEA


# acuracia
AC <- sum(diag(tabela1))/ sum(tabela1)
AC

