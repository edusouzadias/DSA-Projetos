#Projeto 01 - Curso BigDataRAzure da DSA (parte da Formação cientista de dados)
#Detecção de Fraudes em cliques de propaganda

#Definindo diretório de trabalho
setwd("C:/Cursos/FCD/01-BigDataRAzure/Cap20-ProjetosFeedback/Projeto01-DeteccaodeFaudePropaganda")
getwd()

#Carregando alguns pacotes básicos
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(lubridate)
library(data.table)
source("Tools.R")

#Carregando o dataset de treino
df <- fread("train_sample.csv", colClasses=c(rep('factor', 5), 'character', rep('factor',2)))
View(df)
summary(df)

#Primeiramente, vou retirar do dataset a coluna ip
#que não representa uma informação útil para o modelo, sendo apenas uma
#identificação do usuário, assim como a coluna attributed_time 
#que indica o horário do download, sendo uma variável pós fato
df$ip <- NULL
df$attributed_time <- NULL
View(df)

#Procurando por valores NA
qtNA_df <- df[rowSums(is.na(df)) > 0,]
qtNA_df
rm(qtNA_df)

#Como não há valores NA, quebrar a data em dia, hora e minuto
CriaHoraMinDia <- function(df){
  df <- mutate(df,hora = as.factor(hour(df$click_time)))
  df <- mutate(df,dia = as.factor(day(df$click_time)))
  df <- mutate(df,min = as.factor(minute(df$click_time)))
  return(df)
}
df <- CriaHoraMinDia(df)
str(df)

#Vamos verificar a distribuicao de classes
df %>%
  group_by(is_attributed) %>%
  summarise(total = n()/nrow(df)*100) %>%
  View()
#Existem poucas classes 1 (apenas 0.22%). Teremos que arrumar isso no futuro para o treinamento do modelo

#Verificando as variáveis
#Criandos subdataset com downloads efetuados apenas
df_downloaded <- filter(df, is_attributed == 1)

#App
  PlotarAtributos("app", df_downloaded$app)
  #O app 19, 29 e 35 concentram grande parte dos downloads da base, mesmo sendo
  #minoria na base total. Isso indica uma forte relação entre estes app e o download

#Device
  PlotarAtributos("device", df_downloaded$device)
  #O device 0 representa grande parte dos downloads, mesmo aparecendo pouco na base

#OS
  PlotarAtributos("os", df_downloaded$os)
  #O os 0 e 24 representam boa parte dos downloads, mesmo aparecendo pouco na base
  #O restante parece exibir mesma frequencia

#channel
  PlotarAtributos("channel", df_downloaded$channel)
  #O channels 101, 113 e 213 representam boa parte dos downloads, mesmo aparecendo pouco na base

#click_time
  PlotarAtributos("hora", df_downloaded$hora)
  PlotarAtributos("dia", df_downloaded$dia)
  PlotarAtributos("min", df_downloaded$min)
  #Nao parece haver grande correlacao entre o dia ou hora do download, sendo a
  #distribuicao das classes parececidas, porem os minutos parecem apresentar
  #alguma relacao

#MODELO
library(randomForest)
library(caret)

#Separei o dataset em treino, validacao e teste. Como o dataset esta desbalanceado (predominancia da classe 0)
#e temos uma base muito grande, vou gerar o dataset de treino e validacao balanceados baseados na quantidade de
#classes 1, gerando as classes 0 randomicamente. Isso reduzira o numero de linhas totais para treino, porem ainda
#mantendo uma quantidade suficiente de exemplos para treinamento. Ja o dataset de test eu deixarei com a mesma
#distribuicao original, dado que ele é a representação original do dataset, onde o modelo seria aplicado em produção 

#A separacao do dataset foi feita no script GenerateDataSet.R
#Apos separacao dos datasets, foi necessario agrupar os elementos
#dada a grande quantidade de variaveis diferentes do tipo fator,
#o que prejudica a criacao do modelo. O agrupamento foi feito
#no script GenerateDataSetsGroup.R. Estes datasets agrupados
#sao as bases finais que utilizaremos para criacao do modelo
  
trainSet <- fread("trainSetGroup.csv", colClasses=c(rep('factor', 5), rep('integer', 3)))
validationSet <- fread("validationSetGroup.csv", colClasses=c(rep('factor', 5), rep('integer', 3)))
testSet <- fread("testSetGroup.csv", colClasses=c(rep('factor', 5), rep('integer', 3)))

#Nas fases de teste, verifiquei durante o Feature Selection com Random Forest que
#a variavel dia representava pouca importancia para o modelo. Com isso, vou apaga-la da base

trainSet$dia <- NULL
validationSet$dia <- NULL
testSet$dia <- NULL
trainSet <- distinct(trainSet)
validationSet <- distinct(validationSet)
testSet <- distinct(testSet)

levels(validationSet$device) <- levels(trainSet$device)

#Treinando o modelo
modelo <- randomForest( is_attributed ~ ., 
                        data = trainSet)
previsoes <- data.frame(observado = validationSet$is_attributed,
                        previsto = predict(modelo, newdata = validationSet))
confusionMatrix(previsoes$observado, previsoes$previsto)
varImpPlot(modelo)

#Apesar da boa acuracia, a taxa de falsos positivos esta muito alta 63%
#Tentarei agora o modelo de regressao logistica
modeloRL <- glm(formula = is_attributed ~ ., data = trainSet, family = "binomial")
previsoesRL <- data.frame(observado = validationSet$is_attributed,
                        previsto = as.factor(round(predict(modeloRL, newdata = validationSet, type="response"))))
confusionMatrix(previsoesRL$observado, previsoesRL$previsto)

#A regressao logistica teve o mesmo problema do Random Florest, muitos falsos positivos
#Vou testar agora o modelo de Random Florest com o dataset de teste
levels(testSet$device) <- levels(trainSet$device)
previsoes <- data.frame(observado = testSet$is_attributed,
                        previsto = predict(modelo, newdata = testSet))
confusionMatrix(previsoes$observado, previsoes$previsto)

#A acuracia aumentou um pouco, porem fruto do desbalanceamento dos dados (98% das classes 0).
