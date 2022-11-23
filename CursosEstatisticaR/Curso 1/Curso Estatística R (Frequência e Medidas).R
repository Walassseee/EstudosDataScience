# Códigos Básicos

sessionInfo()

install.packages('dplyr')

library(dplyr)

?select

?arrange

# Lendo base de dados

dados <- read.csv('dados.csv')

dados

head(dados, 5)

# Variáveis qualitativas ordinais

select(dados, Anos.de.Estudo)

unique(select(dados, Anos.de.Estudo))

arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo)

c(arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo))

# Variáveis qualitativas nominais

c(arrange(unique(select(dados, Sexo)), Sexo))

c(arrange(unique(select(dados, UF)), UF))

c(arrange(unique(select(dados, Cor)), Cor))


# Variáveis quantitativas discretas

sprintf('De %s até %s anos', min(dados$Idade), max(dados$Idade))

library(glue)

glue('De {min(dados$Idade)} até {max(dados$Idade)} anos')

# Variáveis quantitativas continuas

glue('De {min(dados$Altura)} até {max(dados$Altura)} metros')

# Começando a análise descritiva de dados

# Aula 1 - Distribuição de frequencia 1 variável
  
  table(dados$Sexo)
  
  prop.table(table(dados$Sexo)) * 100
  
  dist_freq_qualitativas <- cbind(freq = table(dados$Sexo), precent = prop.table(table(dados$Sexo)) * 100)
  
  dist_freq_qualitativas
  
  colnames(dist_freq_qualitativas) <- c('Frequencia', 'Porcentagem (%)')
  
  rownames(dist_freq_qualitativas) <- c('Masculino', 'Feminino')
  
  dist_freq_qualitativas

# Aula 2 - Distribuição de frequencia "n" variável
  
  frequencia <- table(dados$Sexo, dados$Cor)
  
  frequencia
  
  rownames(frequencia) <- c('Masculino','Feminino')
  
  colnames(frequencia) <- c('Indigena','Branca','Preta','Amarela','Parda')

  frequencia

  percentual <- prop.table(frequencia) * 100
  
  percentual

  medias <- tapply(dados$Renda, list(dados$Sexo, dados$Cor), mean)

  rownames(medias) <- c('Masculino','Feminino')
  
  colnames(medias) <- c('Indigena','Branca','Preta','Amarela','Parda')

  medias
  
# Aula 3 - Distribuição de frequencia classes personalizadas
  
  min(dados$Renda)

  max(dados$Renda)

  classes <- c(0,1576,3152,7880,15760,200000)
  
  labels <- c('E','D','C','B','A')

  frequencia <- table(
    cut(
      x = dados$Renda,
      breaks = classes,
      labels = labels,
      include.lowest = TRUE
    )
  )
  
  frequencia
  
  percentual <- prop.table(frequencia) * 100

  percentual

  dist_freq_quantitativas_personalizadas <- cbind('Frequencia' = frequencia, 'Porcentagem (%)' = percentual)

  dist_freq_quantitativas_personalizadas
  
  dist_freq_quantitativas_personalizadas[order(row.names(dist_freq_quantitativas_personalizadas)),]
  
# Aula 4 - Distribuição de frequencia amplitude fixa
  
  n <- nrow(dados)

  n  

  k <- 1 + (10 / 3) * log10(n)  

  k <- round(k)

  k  
  
  labels <- c(
    '      0.00 |—|  11,764.70', 
    ' 11,764.70  —|  23,529.40', 
    ' 23,529.40  —|  35,294.10', 
    ' 35,294.10  —|  47,058.80', 
    ' 47,058.80  —|  58,823.50', 
    ' 58,823.50  —|  70,588.20', 
    ' 70,588.20  —|  82,352.90', 
    ' 82,352.90  —|  94,117.60', 
    ' 94,117.60  —| 105,882.00', 
    '105,882.00  —| 117,647.00', 
    '117,647.00  —| 129,412.00', 
    '129,412.00  —| 141,176.00', 
    '141,176.00  —| 152,941.00', 
    '152,941.00  —| 164,706.00', 
    '164,706.00  —| 176,471.00', 
    '176,471.00  —| 188,235.00', 
    '188,235.00  —| 200,000.00'
  )
  
  frequencia <- table(
    cut(
      x = dados$Renda,
      breaks = k,
      labels = labels,
      include.lowest = TRUE
    )
  )
  
  frequencia
  
  percentual <- prop.table(frequencia) * 100
  
  percentual

  dist_freq_quantitativas_amplitude_fixa <- cbind('Frequencia' = frequencia, 'Porcentagem (%)' = percentual)  

  dist_freq_quantitativas_amplitude_fixa  
  
# Aula 5 - Construção de histplot e barplot
  
  options(repr.plot.width = 7, repr.plot.heigh = 4)
  
  hist(dados$Altura)
  
  hist(
    x = dados$Altura,
    breaks = 'Sturges',
    col = 'lightblue',
    main = 'Histograma das Alturas',
    xlab = 'Altura',
    ylab = 'Frequências',
    prob = TRUE,
    las = 1
  )
  
  install.packages('ggplot2')
  
  library(ggplot2)
  
  formatos <- theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.y = element_text(size = 12, vjust = +0.2),
    axis.title.x = element_text(size = 12, vjust = -0.2),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  )
  
  ggplot(dados, aes(x = dados$Altura, y = ..density..)) + 
    geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
    geom_density(color = 'green') +
    ylab("Frequência") + 
    xlab("Alturas") + 
    ggtitle('Histograma das Alturas') +
    formatos

  bar_chart <- data.frame(dist_freq_quantitativas_personalizadas)

  bar_chart 
  
  ggplot(bar_chart, aes(x = row.names(bar_chart), y = bar_chart$Frequencia)) + 
    geom_bar(stat = "identity") + 
    ylab("Frequência") + 
    xlab("Classes de Renda") + 
    ggtitle('Gráfico Classes de Renda') +
    formatos
  
# Medias de tendencia central
  
materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química')

Fulano <- c(8, 10, 4, 8, 6, 10, 8)

Beltrano <- c(10, 2, 0.5, 1, 3, 9.5, 10)

Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7)

df <- data.frame(Fulano, Beltrano, Sicrano, row.names = materias)

df
  
  # Média
  
  mean(df$Fulano)  
  
  mean(dados$Renda)  
  
  aggregate(list(Renda = (dados$Renda)), list(Sexo = dados$Sexo), mean)

  # Mediana
  
  median(df$Fulano)
  
  set.seed(101)

  sample(nrow(df), 6)  

  df_beltrano <- df[sample(nrow(df), 6),]
  
  df_beltrano
  
  median(df$Beltrano)
  
  median(dados$Renda)
  
  aggregate(list(Renda = (dados$Renda)), list(Sexo = dados$Sexo), median)
  
  # Moda
  
  exemplo_moda <- c(1,2,2,3,4,4,5,6,7,7)
  
  freq <- table(exemplo_moda)
  
  freq
  
  freq[freq == max(freq)]
  
  names(freq)[freq == max(freq)]
  
  moda <- function(x){
    freq <- table(x)
    return(names(freq)[freq == max(freq)])
  }
  
  
  moda(exemplo_moda)
  
  moda(df$Fulano)
  
  moda(dados$Renda)
  
  moda(dados$Altura)
  
  
# Relações de medias centrais
  
ggplot(dados[dados$Renda < 20000, ], aes(x = Renda, y = ..density..)) + 
  geom_histogram(binwidth = 500) + 
  geom_density(color = 'green')

# Medidas separatrizes

quantile(dados$Renda, c(0.25,0.5,0.75))

decis <- c()

for(i in 1:9){
  decis <- c(decis, i / 10)
}  
  
quantile(dados$Renda, decis)  
  
ggplot(data = dados, aes(x = dados$Idade)) + 
  geom_histogram(
    aes(y = cumsum(..count..)/sum(..count..)), 
    bins = 10
  ) + 
  geom_freqpoly(
    aes(y = cumsum(..count..)/sum(..count..)), 
    color = 'green'
  )

(length(dados$Idade[dados$Idade <= 40]) / length(dados$Idade)) * 100

# Construindo Boxplot

sexo = c(
  'Masculino', 
  'Feminino'
)
cor = c(
  'Indígena', 
  'Branca', 
  'Preta', 
  'Amarela', 
  'Parda'
)
anos_de_estudo = c(
  'Sem instrução e menos de 1 ano', 
  '1 ano', 
  '2 anos', 
  '3 anos', 
  '4 anos', 
  '5 anos', 
  '6 anos', 
  '7 anos', 
  '8 anos', 
  '9 anos', 
  '10 anos', 
  '11 anos', 
  '12 anos', 
  '13 anos', 
  '14 anos', 
  '15 anos ou mais', 
  'Não determinados'
)
  
ggplot(data = dados, aes(x = "", y = dados$Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Metros") + 
  xlab("") + 
  ggtitle('Box-plot Alturas') +
  formatos  
  
ggplot(data = dados, aes(x = dados$Sexo, y = dados$Altura, group = dados$Sexo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos
  
dados$Cat.sexo <- factor(dados$Sexo)

ggplot(data = dados, aes(x = dados$Cat.sexo, y = dados$Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos  
  
levels(dados$Cat.sexo) <- sexo  
  
ggplot(data = dados, aes(x = dados$Cat.sexo, y = dados$Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos  
  
# Medidas de dispersão

summary(df)
  
install.packages('DescTools')

library(DescTools)  

MeanAD(df$Fulano)  

var(df$Fulano)

sqrt(var(df$Fulano))

sd(df$Fulano)










