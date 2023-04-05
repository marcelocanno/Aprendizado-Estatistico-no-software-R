

## Baixando library
library(MASS)
library(caret)

## Analise de dados Iris ( analise descriminatoria
data(iris)
str(iris)
head(iris, 3)

# validacao linear
r <- lda(formula = Species ~ .,
         data = iris,
         prior = c(1,1,1)/3, CV=TRUE)

# Obter classificacao
clasificacao <- r$class
cv1 <- table(iris$Species, clasificacao)
cv1

# taxa de erro aparente
TEA <- 1- (sum(diag(cv1))/sum(cv1))
TEA
  

# funcao quadratica
q <-  qda(formula = Species ~ .,
              data=iris,
              prior = c(1,1,1)/3,  CV=TRUE)


classificacao <- q$class
cvq <- table(iris$Species,q$class)
cvq

# taxa de erro aparente
TEA <- 1- (sum(diag(cvq))/sum(cvq))
TEA






