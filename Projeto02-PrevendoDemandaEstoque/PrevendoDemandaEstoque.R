#Projeto 02 - Curso BigDataRAzure da DSA (parte da Formação cientista de dados)
#Prevendo demanda de produtos (Grupo Bimbo)

#Definindo diretório de trabalho
setwd("C:/Cursos/FCD/01-BigDataRAzure/Cap20-ProjetosFeedback/Projeto02-PrevendoDemandaEstoque")
getwd()

library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(corrplot)
library(caret)
library(Metrics)

#BASE DE DADOS

df <- fread("train.csv")

#Criando um dataset sample para análise exploratória
df <- df[sample.int(nrow(df),100000),]

#Criando uma coluna com a demanda em pesos
df$Demanda_equil <- ifelse((df$Venta_hoy-df$Dev_proxima)>0,df$Venta_hoy-df$Dev_proxima,0)

#Verificando a base de clientes
dfClientes <- fread("cliente_tabla.csv", encoding = "UTF-8")
length(unique(dfClientes$Cliente_ID))
length(unique(dfClientes$NombreCliente))
#Como o Cliente_ID é chave forte, ainda que duplicado em alguns casos, 
#é melhor identificador que o nome. Com isso, irei trazer o nome apenas para nos ajudar
#na análise exploratória, mas nao o utilizarei no modelo
dfClientes <- dfClientes %>%
  group_by(Cliente_ID) %>%
  summarise(first(NombreCliente))
df <- left_join(df,dfClientes,by="Cliente_ID")
names(df)[names(df) == "first(NombreCliente)"] <- "NombreCliente"
rm(dfClientes)

#Verificando base de produtos
dfProdutos <- fread("producto_tabla.csv", encoding = "UTF-8")
length(unique(dfProdutos$Producto_ID))
length(unique(dfProdutos$NombreProducto))
#Não há IDs e nomes iguais. Com isso, irei trazer o nome dos produtos para a tabela,
#apenas para nos ajudar na análise exploratória
df <- left_join(df,dfProdutos,by="Producto_ID")
rm(dfProdutos)

#Verificano base de cidade e estado
dfTownState <- fread("town_state.csv", encoding = "UTF-8")
length(unique(dfTownState$Agencia_ID))
length(unique(dfTownState$Town))
length(unique(dfTownState$State))
#Nao ha IDs e noms iguais. Com isso, irei trazer o nome dos produtos para a tabela,
#apenas para nos ajudar na analise exploratoria
df <- left_join(df,dfTownState,by="Agencia_ID")
rm(dfTownState)

#Procurando por valores NA
qtNA_df <- df[rowSums(is.na(df)) > 0,]
qtNA_df
rm(qtNA_df)

#Transformando variáveis em categoricas
df$Semana <- as.factor(df$Semana)
df$Agencia_ID <- as.factor(df$Agencia_ID)
df$Canal_ID <- as.factor(df$Canal_ID)
df$Producto_ID <- as.factor(df$Producto_ID)
df$Ruta_SAK <- as.factor(df$Ruta_SAK)
df$Cliente_ID <- as.factor(df$Cliente_ID)

#Análise básica dos dados
summary(df)
str(df)
head(df)
View(df)

#ANÁLISE EXPLORATÓRIA

#Distribuição dos dados
g <- ggplot(df)

p1 <- g + geom_histogram(aes(Venta_uni_hoy), binwidth = 1) + coord_cartesian(xlim = c(0,30)) + labs(title="Histograma de demanda", x="Demanda (unitária) última semana")
p2 <- g + geom_histogram(aes(Venta_hoy), binwidth = 5) + coord_cartesian(xlim = c(0,150)) + labs(title="Histograma de demanda", x="Demanda (pesos) última semana")
p3 <- g + geom_histogram(aes(Dev_uni_proxima), binwidth = 1) + coord_cartesian(xlim = c(0,5)) + labs(title="Histograma de retornos", x="Retorno (unitária)")
p4 <- g + geom_histogram(aes(Dev_proxima), binwidth = 1) + coord_cartesian(xlim = c(0,5)) + labs(title="Histograma de retornos", x="Retorno (pesos)")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
#Demanda muito concentrada (parecendo uma distribuição exponencial). Os retornos também
#concentrados, com muitos registros sem retorno. A base possui muitos outlyers, o que dificulta
#a visualizacao dos gráficos (precisa limitar os eixos)

#Dias da semana
p1 <- g + geom_boxplot(aes(x=Semana, y=Venta_uni_hoy)) + coord_cartesian(ylim = c(0,10)) + labs(title="Boxplot de demanda",x="Dia da semana", y="Demanda (unitária) última semana")
p2 <- g + geom_boxplot(aes(x=Semana, y=Venta_hoy)) + coord_cartesian(ylim = c(0,75)) + labs(title="Boxplot de demanda", x="Dia da semana", y="Demanda (pesos) última semana")
p3 <- g + geom_bar(aes(Semana, fill=State)) + labs(title="Dia de semana por estado", x="Dia da semana", y="Frequência")
p4 <- g + geom_bar(aes(Semana, fill=Canal_ID)) + labs(title="Dia de semana por canal", x="Dia da semana", y="Frequência")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
#Demanda parece uniforme ao longo dos dias, assim como o número de aparições de cada dia e sua
#localização e canal de venda (concentrada no ID 1).

#Canal de venda
p1 <- g + geom_boxplot(aes(x=Canal_ID, y=Venta_uni_hoy)) + coord_cartesian(ylim = c(0,300)) + labs(title="Boxplot de demanda",x="Canal de venda", y="Demanda (unitária) última semana")
p2 <- g + geom_boxplot(aes(x=Canal_ID, y=Venta_hoy)) + coord_cartesian(ylim = c(0,2000))+ labs(title="Boxplot de demanda", x="Canal de venda", y="Demanda (pesos) última semana")
p3 <- g + geom_bar(aes(Canal_ID, fill=State)) + labs(title="Canal de venda por estado", x="Canal de venda", y="Frequência")
grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2),ggplotGrob(p3)))
#Apesar do ID 1 concentrar a maior parte dos registros na base e possuir pouca variabilidade
#na demanda, outros canais como 2 e 5 apresentam muita variabilidade. 
#Já é possível também perceber concentração de alguns estados na base (cores em tom de verde)

#Cliente
p1 <- df %>%
  group_by(NombreCliente) %>%
  summarize(Venta_uni_hoy = sum(Venta_uni_hoy)) %>%
  arrange(desc(Venta_uni_hoy)) %>%
  top_n(5) %>%
  ggplot + geom_bar(aes(x=NombreCliente, y=Venta_uni_hoy),stat="identity")  + labs(title="Demanda por cliente (TOP 5)",x="Cliente", y="Demanda (unitária) última semana")
p2 <- df %>%
  group_by(NombreCliente) %>%
  summarize(Venta_uni_hoy = sum(Venta_uni_hoy)) %>%
  arrange(desc(Venta_uni_hoy)) %>%
  slice(5:2000) %>%
  ggplot + geom_bar(aes(x=reorder(NombreCliente,-Venta_uni_hoy), y=Venta_uni_hoy),stat="identity")  + labs(title="Demanda por cliente",x="Cliente", y="Demanda (unitária) última semana")
p3 <- df %>%
  group_by(State) %>%
  summarize(Clientes = n_distinct(NombreCliente)) %>%
  ggplot + geom_bar(aes(x=State, y=Clientes),stat="identity") + theme(axis.text.x = element_text(angle = 90)) + labs(title="Clientes por estado",x="Estado", y="Cliente")
p4 <- df %>%
  group_by(Semana) %>%
  summarize(Clientes = n_distinct(NombreCliente)) %>%
  ggplot + geom_bar(aes(x=Semana, y=Clientes),stat="identity")+ labs(title="Clientes por dia da semana",x="Dia da semana", y="Cliente")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
#Grande parte da demanda está em clientes não identificados, como é possível ver no top 5 clientes.
#O restante apresenta um decaimento que parece exponencial. Alguns estados possuem muito mais
#clientes que outro. Já a divisão por dia da semana parece uniforme

#Estado e cidade
p1 <- df %>%
  group_by(State) %>%
  summarize(Venta_uni_hoy = sum(Venta_uni_hoy)) %>%
  arrange(desc(Venta_uni_hoy)) %>%
  ggplot + geom_bar(aes(x=State, y=Venta_uni_hoy),stat="identity") + theme(axis.text.x = element_text(angle = 90))  + labs(title="Demanda por estado",x="Estado", y="Demanda (unitária) última semana")
p2 <- g + geom_bar(aes(State))+ theme(axis.text.x = element_text(angle = 90)) + theme(axis.text.x = element_text(angle = 90))  + labs(title="Distribuição dos estados",x="Estado", y="Frequência")
p3 <- df %>%
  group_by(Town) %>%
  summarize(Venta_uni_hoy = sum(Venta_uni_hoy)) %>%
  arrange(desc(Venta_uni_hoy)) %>%
  ggplot + geom_bar(aes(x=reorder(Town,-Venta_uni_hoy), y=Venta_uni_hoy),stat="identity") + theme(axis.text.x = element_text(angle = 90)) + theme(axis.text.x = element_text(angle = 90))  + labs(title="Demanda por cidade",x="Cidade", y="Demanda (unitária) última semana")
p4 <- g + geom_bar(aes(x=reorder(Town,-table(Town)[Town]))) + theme(axis.text.x = element_text(angle = 90))  + labs(title="Distribuição das cidades",x="Cidade", y="Frequência")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
#A demanda segue a aparicão na base das cidades e estados. É possível verificar que poucas cidades
#e estados dominam a demanda. Três estados (Estado do Mexico, Jalisco e Mexico DF) representam
#as maiores demandas

#Produto
p1 <- df %>%
  group_by(NombreProducto) %>%
  summarize(Venta_uni_hoy = sum(Venta_uni_hoy)) %>%
  arrange(desc(Venta_uni_hoy)) %>%
  ggplot + geom_bar(aes(x=reorder(NombreProducto,-Venta_uni_hoy), y=Venta_uni_hoy),stat="identity") + theme(axis.title.x=element_blank(),
                                                                                                            axis.text.x=element_blank(),
                                                                                                            axis.ticks.x=element_blank())  + labs(title="Demanda por produto", y="Demanda (unitária) última semana")
p2 <- g + geom_bar(aes(x=reorder(NombreProducto,-table(NombreProducto)[NombreProducto])))+ theme(axis.title.x=element_blank(),
                                                                                                 axis.text.x=element_blank(),
                                                                                                 axis.ticks.x=element_blank())  + labs(title="Distribuição por produto", y="Frequência")
p3 <- df %>%
  group_by(NombreProducto) %>%
  summarize(Venta_uni_hoy = sum(Venta_uni_hoy)) %>%
  arrange(desc(Venta_uni_hoy)) %>%
  slice(1:20) %>%
  ggplot + geom_bar(aes(x=reorder(NombreProducto,-Venta_uni_hoy), y=Venta_uni_hoy),stat="identity") + theme(axis.text.x = element_text(angle = 90))  + labs(title="Demanda por produto (TOP 20)",x="Produto", y="Demanda (unitária) última semana")
p4 <- df %>%
  group_by(NombreProducto) %>%
  summarize(contar = n()) %>%
  arrange(desc(contar)) %>%
  slice(1:20) %>%
  ggplot + geom_bar(aes(x=reorder(NombreProducto,-contar), y=contar),stat="identity") + theme(axis.text.x = element_text(angle = 90))  + labs(title="Distribuição dos produtos",x="Produto", y="Frequência")
grid.draw(cbind(rbind(ggplotGrob(p1), ggplotGrob(p2)),rbind(ggplotGrob(p3),ggplotGrob(p4))))
#Apesar de alguns produtos concentrarem grande parte da demanda, estes não são o que aparecem mais
#na base. Isto é, alguns produtos quando demandados o fazem em grande quantidade, mesmo que em
#pouca frequência, como pode se visto nos gráficos dos 20 produtos mais demandados em comparação
#aos 20 que mais aparecem na base

#Demanda anterior com demanda atual
g + geom_point(aes(x=Venta_hoy,y=Demanda_equil)) + coord_cartesian(xlim = c(0,2000),ylim = c(0,2000))  + labs(title="Demanda unitária - última semana e atual",x="Última semana", y="Semana atual")
#As variáveis possuem uma correlacao muito alta, indicando que a demanda da semana anterior
#é fundamental para previsão da demanda atual. Isso ja era esperado dada a grande quantidade
#de semanas onde não há retornos, sendo a demanda atual igual a anterior
cor(df$Venta_hoy,df$Demanda_equil,method="pearson")
correlacoes = cor(df[c("Venta_uni_hoy","Venta_hoy","Dev_uni_proxima","Dev_proxima","Demanda_uni_equil","Demanda_equil")],method="pearson")
corrplot(correlacoes, type="lower")
#Como a correlação entre a demanda unitária e por pesos é muito grande, utilizarei
#apenas a demanda por unidade, afim de evitar multicolinearidade

#MODELO PREDITIVO

#Trazendo novamente o dataset total e adicionando apenas a variável State
df <- fread("train.csv")

#Como a base é muito grande, vou criar uma base menor (1 milhão de linhas) para o projeto
#considerando apenas fins didáticos e a capacidade da minha máquina
df <- df[sample.int(nrow(df),1000000),]

#Adicionando estado
dfTownState <- fread("town_state.csv", encoding = "UTF-8")
df <- left_join(df,dfTownState,by="Agencia_ID")
rm(dfTownState)

#Transformando variáveis em categóricas
df$Semana <- as.factor(df$Semana)
df$Canal_ID <- as.factor(df$Canal_ID)

#Separação treino e teste
colunas <- c("Semana","State","Canal_ID","Venta_uni_hoy","Dev_uni_proxima","Demanda_uni_equil")
trainIndex <- createDataPartition(df$Demanda_uni_equil, p = .7, list = FALSE, times = 1)
trainSet <- df[trainIndex,colunas]
testSet  <- df[-trainIndex,colunas]
rm(df)

#Criando o modelo
modelo <- train(Demanda_uni_equil ~ ., data=trainSet, method="lm")

summary(modelo)
#Pela ANOVA, podemos ver que as variáveis numéricas Venta_uni_hoy e Dev_uni_proxima
#possuem p-values muito pequenos, o que significa que são variáveis importantes
#para explicar a variável de saída. Já quando olhamos as variáveis categóricas,
#o resultado é menos expressivo, porém alguns dias da semana, canais e estados
#possuem p-values bem pequenos (nível de significância de 5%).  

#Verificando a acurácia do modelo
scores <- data.frame(actual = testSet$Demanda_uni_equil,
                     prediction = predict(modelo, newdata = testSet))

#Calculando o erro quadrático logarítmico médio (RMSLE)
scores$prediction <- ifelse((scores$prediction)>0,scores$prediction,0)
rmsle(scores$actual, scores$prediction)

#O rmsle é baixo, mostrando que o modelo conseguiu uma boa acurácia.





