

# Definir o caminho dos dados

setwd("C:/Users/canno/OneDrive/Documentos/")
dados <- read.csv("dados_selecao_variaveis_II.csv", header = T)
head(dados)
str(dados)

# Resumo dos dados

summary(dados)

# Todos possiveis modelos

nMod <- 2^5
nMod

#Realizar a comparacao apenas para alguns

mod1<-lm(c_frango ~ renda + p_frango + p_porco + p_boi + p_outros, data= dados)
mod2<-lm(c_frango ~ renda + p_frango + p_porco + p_boi, data= dados)
mod3<-lm(c_frango ~ renda + p_frango + p_porco , data= dados)
mod4<-lm(c_frango ~ renda + p_frango,data= dados)
mod5<-lm(c_frango ~ renda, data= dados)
AIC <- cbind(AIC(mod1),AIC(mod2), AIC(mod3), AIC(mod4), AIC(mod5))
AIC

# Selecao Automatica (Usando a funcao stepAIC)

library(MASS)

# Metodo Forward
# Modelo Inicial

mod1<- lm(c_frango ~ 1, data=dados)

# Avaliaïcao dos modelos

step1 <- stepAIC(mod1, direction= "forward", scope = list(upper = ~ renda + p_frango + p_porco + p_boi + p_outros, lower = ~ 1),trace= 1)

# Melhor Modelo

step1

# Metodo Backward
# Modelo Inicial

mod2<- lm(c_frango~renda + p_frango + p_porco + p_boi + p_outros, data= dados)
step2 <- stepAIC(mod2, direction= "backward", scope = list(upper = ~ renda + p_frango + p_porco + p_boi + p_outros, lower = ~1),trace= 1)

# Melhor Modelo

step2

# Metodo Stepwise

stepwise <- stepAIC(mod1, direction= "both", scope = list(upper = ~ renda + p_frango + p_porco + p_boi + p_outros, lower = ~1),trace= 1)
stepwise

