install.packages('ggplot2')
install.packages('dplyr')

library(ggplot2)
library(dplyr)

sessionInfo()

dados <- read.csv('C:/Estudos/EstudosDataScience/CursosEstatisticaR/Curso 2/Dados/dados.csv')

head(dados)

# Distribuição de probablidade binomial

combinacoes <- choose(60,6)

combinacoes

probabilidade <- 1 / combinacoes

probabilidade

# Exercicio

  n <- 10

  alternativas <- 3  

  p <- 1 / alternativas
  
  q <- 1 - p

  k <- 5
  
  prob <- choose(n,k) * (p ** k) * (q ** (n - k)) 

  dbinom(x = k, size = n, prob = p)
  
  sum(dbinom(x = k:10, size = n, prob = p))
  
  pbinom(q = 4, size = n, prob = p, lower.tail = FALSE)
  
# Médias de probabilidade binomial
  
p <- 0.6

n <- 12

k <- 8

probabilidade <- dbinom(x = k, size = n, prob = p)

equipes <- 30 * probabilidade

equipes
