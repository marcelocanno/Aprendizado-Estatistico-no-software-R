
# Instalar pacote para acessar os dados e ajuste do modelo de Regressao em Ridge
#install.packages("ridge")
# Carregar o pacote

library(ridge)

# Leitura dos dados

data(GenCont)
str(GenCont)
head(GenCont)

# Ajustando o modelo linear no R

mod <- lm(Phenotypes ~ -1 + GenCont[,2:13], data = as.data.frame(GenCont))
summary(mod)

# Ajuste do modelo de Regressao em Ridge

mod1 <- linearRidge(Phenotypes ~ -1 + GenCont[,2:13], data = as.data.frame(GenCont))
summary(mod1)

# Instalar pacote para o ajuste da Regressao via Componentes Principais
#install.packages("pls")

library(pls)
head(GenCont)

# Ajuste: Regressao via componentes principais

pcr <- pcr(Phenotypes ~ ., data = as.data.frame(GenCont), ncomp = 6, validation = "CV")
summary(pcr)

