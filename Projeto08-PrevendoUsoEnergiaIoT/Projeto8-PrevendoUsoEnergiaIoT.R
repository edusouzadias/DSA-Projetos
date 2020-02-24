# Projeto 08 - Curso MachineLearnig da DSA (parte da Formação cientista de dados)
# Previsão do uso de energia (IoT)

# Definindo diretório de trabalho
setwd("C:/Cursos/FCD/04-Machine-Learning/21-Projetos_com_feedback/Previsao_Uso_de_Energia_IoT")
getwd()

library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(corrplot)
library(caret)
library(Metrics)
library(lubridate)

# Base de Dados
dfTrain <- fread("projeto8-training.csv")
dfTest <- fread("projeto8-testing.csv")
df <- rbind(dfTrain, dfTest)

# Criando variaveis novas
df$Dia <- day(df$date)
df$Mes <- month(df$date)
df$Hora <- hour(df$date)
df$Min <- minute(df$date)

# Convertendo as variáveis para os formatos corretos
df$date <- as_datetime(df$date)
df$dateOnly <- date(df$date)
df$Appliances <- as.integer(df$Appliances)
df$lights <- as.integer(df$lights)
df$T6 <- as.numeric(df$T6)
df$RH_6 <- as.numeric(df$RH_6)
df$T_out <- as.numeric(df$T_out)
df$RH_out <- as.numeric(df$RH_out)
df$Windspeed <- as.numeric(df$Windspeed)
df$Visibility <- as.numeric(df$Visibility)
df$Tdewpoint <- as.numeric(df$Tdewpoint)
df$rv1 <- as.numeric(df$rv1)
df$rv2 <- as.numeric(df$rv2)
df$NSM <- as.integer(df$NSM)
df$Dia <- as.factor(df$Dia)
df$Mes <- as.factor(df$Mes)
df$Hora <- as.factor(df$Hora)
df$Min <- as.factor(df$Min)

str(df)

# Análise exploratória
g <- ggplot(df)

# Datas
p1 <- df %>%
  group_by(WeekStatus) %>%
  summarize(Appliances = mean(Appliances)) %>%
  ggplot + geom_bar(aes(x=WeekStatus, y=Appliances), stat="identity")  + labs(title="WeekStatus", x="WeekStatus", y="Consumo de energia médio (10 min)")
p2 <- df %>%
  group_by(Day_of_week) %>%
  summarize(Appliances = mean(Appliances)) %>%
  ggplot + geom_bar(aes(x=Day_of_week, y=Appliances), stat="identity") + scale_x_discrete(limits=c('Sunday', 'Monday','Thursday','Wednesday','Tuesday','Friday', 'Saturday')) + labs(title="Dia da Semana", x="Dia da Semana", y="Consumo de energia médio (10 min)")
p3 <- df %>%
  group_by(Dia) %>%
  summarize(Appliances = mean(Appliances)) %>%
  ggplot + geom_bar(aes(x=Dia, y=Appliances), stat="identity")  + labs(title="Dia do mês", x="Dia do mês", y="Consumo de energia médio (10 min)")
p4 <- df %>%
  group_by(Hora) %>%
  summarize(Appliances = mean(Appliances)) %>%
  ggplot + geom_bar(aes(x=Hora, y=Appliances), stat="identity")  + labs(title="Hora", x="Hora", y="Consumo de energia médio (10 min)")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
# É possível verificar que a diferença no consumo de energia entre dias úteis e finais de semana é pequena
# Já para o dia da semana, mês e hora, parece existir grande variação no consumo, indicando que devem ser boas
# variáveis preditoras

# Transformando as variáveis de dia da semana em fator
df$Day_of_week[df$Day_of_week == 'Sunday'] <- 1
df$Day_of_week[df$Day_of_week == 'Monday'] <- 2
df$Day_of_week[df$Day_of_week == 'Thursday'] <- 3
df$Day_of_week[df$Day_of_week == 'Wednesday'] <- 4
df$Day_of_week[df$Day_of_week == 'Tuesday'] <- 5
df$Day_of_week[df$Day_of_week == 'Friday'] <- 6
df$Day_of_week[df$Day_of_week == 'Saturday'] <- 7
df$Day_of_week <- as.factor(df$Day_of_week)

# Date
df %>%
  group_by(dateOnly) %>%
  summarize(Appliances = sum(Appliances)) %>%
  ggplot + geom_line(aes(x=dateOnly, y=Appliances), stat="identity") + labs(title="Data", x="Data", y="Consumo de energia")
# O consumo parece variar de forma relevante ao longo do tempo

# Relações variáveis numéricas
p1 <- g + geom_point(aes(x=Press_mm_hg, y=Appliances)) + labs(title="Press_mm_hg",x="Press_mm_hg", y="Consumo de energia médio (10 min)")
p2 <- g + geom_point(aes(x=Windspeed, y=Appliances)) + labs(title="Windspeed", x="Windspeed", y="Consumo de energia médio (10 min)")
p3 <- g + geom_point(aes(x=Visibility, y=Appliances)) + labs(title="Visibility", x="Visibility", y="Consumo de energia médio (10 min)")
p4 <- g + geom_point(aes(x=Tdewpoint, y=Appliances)) + labs(title="Tdewpoint", x="Tdewpoint", y="Consumo de energia médio (10 min)")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
# A relaçoes entre as variáveis não parecem relevantes, o que pode indicar que o modelo não consiga grande
# acurácia na previsão

# Mais Relações variáveis numéricas
p1 <- g + geom_boxplot(aes(x=as.factor(lights), y=Appliances)) + labs(title="lights",x="lights", y="Consumo de energia médio (10 min)")
p2 <- g + geom_point(aes(x=T_out, y=Appliances)) + labs(title="T_out", x="T_out", y="Consumo de energia médio (10 min)")
p3 <- g + geom_point(aes(x=RH_out, y=Appliances)) + labs(title="RH_out", x="RH_out", y="Consumo de energia médio (10 min)")
p4 <- g + geom_point(aes(x=NSM, y=Appliances)) + labs(title="NSM", x="NSM", y="Consumo de energia médio (10 min)")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
# Mais uma vez as variáveis não demostram fortes relações, com exceção da variável lights, que possui alguma
# variabilidade nos dados

# Correlações
variaveis <- c('T1','RH_1','T2','RH_2','T3','RH_3','T4','RH_4','T5','RH_5','T6','RH_6','T7','RH_7','T8','RH_8','T9','RH_9','T_out','RH_out','Press_mm_hg','Windspeed','Visibility','Tdewpoint','rv1','rv2', 'NSM')
correlacoes = cor(df[,..variaveis], method="pearson")
corrplot(correlacoes, type="lower", method = 'number', number.cex = 0.7)
# As variáveis T_out com T6 e T7 com T9 possuem grande correlação, não devendo entrar no modelo juntas
# Com isso, iremos remove-las
df$T6 <- NULL
df$T9 <- NULL
variaveis <- c('T1','RH_1','T2','RH_2','T3','RH_3','T4','RH_4','T5','RH_5','RH_6','T7','RH_7','T8','RH_8','RH_9','T_out','RH_out','Press_mm_hg','Windspeed','Visibility','Tdewpoint','rv1','rv2', 'NSM')

# Modelo
library(randomForest)
library(caret)

# Normalizando os dados
for (item in variaveis){
  X <- df[[item]]
  df[[item]] <- (X - min(X)) / (max(X) - min(X))
}

#Separando o Dataset
trainSet <- df[1:nrow(dfTrain)]
testSet <- df[(nrow(dfTrain)+1):nrow(df)]

#Treinando o modelo
variaveisModelo <- 'Appliances ~ T1+RH_1+T2+RH_2+T3+RH_3+T4+RH_4+T5+RH_5+RH_6+T7+RH_7+T8+RH_8+RH_9+T_out+RH_out+Press_mm_hg+Windspeed+Visibility+Tdewpoint+rv1+rv2+NSM+lights+Dia+Hora+Mes+Min+Day_of_week'
variaveisModelo <- as.formula(variaveisModelo)
modeloRF <- randomForest(variaveisModelo, data = trainSet)
varImpPlot(modeloRF)

# Escolhendo as variáveis mais importantes
variaveisModelo <- 'Appliances ~ Hora + Dia + NSM + RH_3 + Press_mm_hg + RH_2 + T3 + RH_5 + T8 + lights + RH_1 + T2 + RH_out'
variaveisModelo <- as.formula(variaveisModelo)

# Definindo cross validation
ctrl <- trainControl(method = "cv", number=5)

# Treinando o modelo de Regressao Logística Multilinear
modeloRLM <- train(variaveisModelo, data=trainSet, method='glm', metric='Rsquared', trControl=ctrl)
print(modeloRLM)
# Não apresentou performance muito boa

# Treinando o modelo de Support Vector Machines
modeloSVM <- train(variaveisModelo, data=trainSet, method='svmLinear', metric='Rsquared', trControl=ctrl)
print(modeloSVM)
# Também não teve uma performance muito boa. Teremos que utilizar modelos não lineares.

#Treinando o modelo de Stochastic Gradient Boosting
modeloSGB <- train(variaveisModelo, data=trainSet, method='gbm', metric='Rsquared', trControl=ctrl)
print(modeloSGB)
# A performance já melhorou muito, com diminuição do RMSE e aumento do R-squared

#Treinando o modelo de Extreme Gradient Boosting
modeloEGB <- train(variaveisModelo, data=trainSet, method='xgbLinear', metric='Rsquared', trControl=ctrl)
print(modeloEGB)
# Melhor resultado até o momento

#Otimizando o melhor modelo EGB, o qual apresentou melhor resultado
library(xgboost)
ctrl <- trainControl(method = "cv", number=5)
grid <- expand.grid(nrounds = c(100,200),
                    max_depth = c(10,20,30),
                    eta = 0.1,
                    gamma = 0,
                    colsample_bytree = c(0.3,0.5,0.7),
                    min_child_weight = 1,
                    subsample = 1)
modeloEGB <- train(variaveisModelo, data=trainSet, method='xgbTree', metric='Rsquared', trControl=ctrl, tuneGrid=grid)
print(modeloEGB)

# Melhores parâmetros
modeloEGB$bestTune

# Vamos treinar o modelo com os melhores parametros
modeloEGB <- train(variaveisModelo, data=trainSet, method='xgbTree', metric='Rsquared', trControl=ctrl, tuneGrid=modeloEGB$bestTune)
print(modeloEGB)

# Por último, vamos analisar como o modelo se sai com os dados de Teste
previsao <- predict(modeloEGB, testSet)
residuos <- testSet$Appliances - previsao
# RMSE nos dados de Teste
rmse(testSet$Appliances, previsao)
# R-Squared nos dados de Teste
tss <- sum((testSet$Appliances - mean(testSet$Appliances))^2)
rss <- sum(residuos^2)
1-(rss/tss)

# Concluindo, o modelo de Extreme Gradient Boosting apresentou o melhor resultado, utilizando apenas
# 13 das 35 variáveis disponíveis, filtradas pela ordem de importância utilizando um modelo de Random Forest.
# O r-squared, porém, não atingiu um valor alto. Isso já era esperado, dado que os atributos possuem pouca 
# relação com a variável preditora, conforme visto na análise exploratória.