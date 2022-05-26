library(tidyverse)
df = read.csv('googleplaystore.csv')

#Tratamento para a plotagem dos gráficos
categories = c('Ferramentas', 'Familia', 'Médico', 'Jogos', 'Mercado')
df$Category[df$Category=='TOOLS'] = categories[1]
df$Category[df$Category=='FAMILY'] = categories[2]
df$Category[df$Category=='MEDICAL'] = categories[3]
df$Category[df$Category=='GAME'] = categories[4]
df$Category[df$Category=='BUSINESS'] = categories[5]

df_tools = df %>%
  filter(df$Category==categories[1])
df_family = df %>%
  filter(df$Category==categories[2])
df_medical = df %>%
  filter(df$Category==categories[3])
df_game = df %>%
  filter(df$Category==categories[4])
df_business = df %>%
  filter(df$Category==categories[5])

#obtendo a média e mediana de opiniões de cada categoria de aplicativo
means = c(mean(df_tools$Reviews), 
          mean(df_business$Reviews), 
          mean(df_family$Reviews), 
          mean(df_game$Reviews), 
          mean(df_medical$Reviews))
medians = c(median(df_tools$Reviews), 
          median(df_business$Reviews), 
          median(df_family$Reviews), 
          median(df_game$Reviews), 
          median(df_medical$Reviews))

options(scipen=999) # para tirar as notações científicas dos gráficos

#criação do boxplot de avaliações por categoria
boxplot = ggplot(df, aes(y = df$Rating, x = df$Category)) +
  geom_boxplot(fill = 'blue', color = 'black') +
  ylab('AVALIAÇÕES') +
  xlab('CATEGORIAS') +
  ggtitle('DISTRIBUIÇÃO DAS AVALIAÇÕES DO APLICATIVO POR CATEGORIA') +
  theme_classic()



#gráfico da media de numero de avaliações (interesse) por categoria
df_bar = data.frame(médias = means,
                    categorias = categories,
                    medianas = medians)

ggplot(df_bar, aes(y = df_bar$médias, x = df_bar$categorias)) +
  geom_bar(stat = "identity", fill = 'blue', color = 'black') +
  xlab('CATEGORIAS') +
  ylab('MÉDIAS') +
  ggtitle('MÉDIA DO NÚMERO DE AVALIAÇÕES POR CATEGORIA') +
  theme_classic()
print(barplot)

#gráfico da media de numero de avaliações (interesse) por categoria
ggplot(df_bar, aes(y = df_bar$medianas, x = df_bar$categorias)) +
  geom_bar(stat = "identity", fill = 'blue', color = 'black') +
  xlab('CATEGORIAS') +
  ylab('MEDIANAS') +
  ggtitle('MEDIANA DO NÚMERO DE AVALIAÇÕES POR CATEGORIA') +
  theme_classic()

#usaremos agora somente o df_game

#grafico de numero de avaliações por gratuidade
df_g1 = data.frame(gratuidade = c('Gratuitos', 'Pagos'),
                  medias = c(mean(df_game$Reviews[df_game$Type == "Free"]),
                             mean(df_game$Reviews[df_game$Type == "Paid"])
                             )
                  )

ggplot(df_g1, aes(y = df_g1$medias, x = df_g1$gratuidade)) +
  geom_bar(stat = "identity", fill = 'blue', color = 'black') +
  xlab('GRATUIDADE') +
  ylab('MÉDIAS') +
  ggtitle('MÉDIA DO NÚMERO DE AVALIAÇÕES POR GRATUIDADE') +
  theme_classic()

#grafico de numero de avaliações por classificação
df_g2 = data.frame(classificações = c('10+', '17+', 'Todos', 'Adolecentes'),
                   medias = c(mean(df_game$Reviews[df_game$Content.Rating == "Everyone 10+"]),
                              mean(df_game$Reviews[df_game$Content.Rating == "Mature 17+"]),
                              mean(df_game$Reviews[df_game$Content.Rating == "Everyone"]),
                              mean(df_game$Reviews[df_game$Content.Rating == "Teen"]
                              )
                   )
)

ggplot(df_g2, aes(y = df_g2$medias, x = df_g2$classificações)) +
  geom_bar(stat = "identity", fill = 'blue', color = 'black') +
  xlab('CLASSIFICAÇÕES') +
  ylab('MÉDIAS') +
  ggtitle('MÉDIA DO NÚMERO DE AVALIAÇÕES POR CLASSIFICAÇÕES DE CONTEÚDO') +
  theme_classic()

#grafico de satisfação por gratuidade
df_g3 = data.frame(gratuidade = c('Gratuitos', 'Pagos'),
                   medias = c(mean(df_game$Rating[df_game$Type == "Free"]),
                              mean(df_game$Rating[df_game$Type == "Paid"])
                   )
)

ggplot(df_g3, aes(y = df_g3$medias, x = df_g3$gratuidade)) +
  geom_bar(stat = "identity", fill = 'blue', color = 'black') +
  xlab('SATISFAÇÃO') +
  ylab('MÉDIAS') +
  ggtitle('MÉDIA DO NÍVEL DE SATISFAÇÃO POR GRATUIDADE') +
  theme_classic()

#grafico de satisfação por classificação de conteúdo
df_g4 = data.frame(classificações = c('10+', '17+', 'Todos', 'Adolecentes'),
                   medias = c(mean(df_game$Rating[df_game$Content.Rating == "Everyone 10+"]),
                              mean(df_game$Rating[df_game$Content.Rating == "Mature 17+"]),
                              mean(df_game$Rating[df_game$Content.Rating == "Everyone"]),
                              mean(df_game$Rating[df_game$Content.Rating == "Teen"]
                              )
                   )
)

ggplot(df_g4, aes(y = df_g4$medias, x = df_g4$classificações)) +
  geom_bar(stat = "identity", fill = 'blue', color = 'black') +
  xlab('CLASSIFICAÇÕES') +
  ylab('MÉDIAS') +
  ggtitle('MÉDIA DO NÍVEL DE SATISFAÇÃO POR CLASSIFICAÇÕES DE CONTEÚDO') +
  theme_classic()