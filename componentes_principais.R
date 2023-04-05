
# Analise de componentes principais

# Caminho dos dados
setwd("C:/Users/canno/OneDrive/Documentos/")
dados <- read.csv("componentes_principais.csv")
dados
str(dados)

# Obtencao da matriz de covariancia
S <- var(dados[,-1])
S

# Obtencao dos autovalores e autovetores
autos <- eigen(S)
autos

# Obtencao de escores
dad <- as.matrix(dados[,-1])
escores1 <- dad%*%autos$vectors
escores1

# Percentual de explicacao
percent_expli <- autos$values/sum(diag(S))
percent_expli

# Apresentacao
id <-dados[,1]
cp_plot<-cbind(id,escores1)
plot(cp_plot[,2], cp_plot[,3], type = "n", xlab = "Componente Principal 1 ",
     ylab = "Componente Principal 2")
text(y=cp_plot[,3], x=cp_plot[,2])

