library(caret)
library(data.table)
library(dplyr)

#Carregando o novo dataset gerado com as bases balanceadas
trainSet <- fread("trainSet.csv", colClasses=c(rep('factor', 4), 'character', 'factor',rep('integer', 3)))
validationSet <- fread("validationSet.csv", colClasses=c(rep('factor', 4), 'character', 'factor',rep('integer', 3)))
testSet <- fread("testSet.csv", colClasses=c(rep('factor', 4), 'character', 'factor',rep('integer', 3)))

df <- rbind(trainSet,validationSet)
df_downloaded <- filter(df, is_attributed == 1)

#App
#Agrupando
AtributosPoucoUtilizados("app")
appGroup <- unique(df_downloaded$app)
temp_df <- df_downloaded %>%
  group_by(app) %>%
  summarise(totalDownloaded = n())

temp_df <- df %>%
  group_by(app) %>%
  summarise(total = n()) %>%
  filter(app %in% appGroup) %>%
  left_join(temp_df) %>%
  mutate(percDownloaded = totalDownloaded/total) %>%
  arrange(desc(percDownloaded))

j <- -1
for (i in seq(0.95, 0, -0.05)) {
  temp <- temp_df[temp_df$percDownloaded > i,'app']
  levels(trainSet$app)[levels(trainSet$app) %in% temp$app] <- j
  levels(validationSet$app)[levels(validationSet$app) %in% temp$app] <- j
  levels(testSet$app)[levels(testSet$app) %in% temp$app] <- j
  j <- j - 1
}

temp_df <- df %>%
  group_by(app) %>%
  summarise(total = n()) %>%
  filter(!app %in% appGroup) %>%
  arrange(desc(total)) %>%
  select(app) %>%
  slice(1:5)
levels(trainSet$app)[!levels(trainSet$app) %in% temp_df$app & !levels(trainSet$app) %in% seq(-1,-20,-1)] <- -21
levels(validationSet$app)[!levels(validationSet$app) %in% temp_df$app & !levels(validationSet$app) %in% seq(-1,-20,-1)] <- -21
levels(testSet$app)[!levels(testSet$app) %in% temp_df$app & !levels(testSet$app) %in% seq(-1,-20,-1)] <- -21

#Device
#Agrupando
AtributosPoucoUtilizados("device")
deviceGroup <- unique(df_downloaded$device)

temp_df <- df_downloaded %>%
  group_by(device) %>%
  summarise(totalDownloaded = n())

temp_df <- df %>%
  group_by(device) %>%
  summarise(total = n()) %>%
  filter(device %in% deviceGroup) %>%
  left_join(temp_df) %>%
  mutate(percDownloaded = totalDownloaded/total) %>%
  arrange(desc(percDownloaded))

j <- -1
for (i in seq(0.95, 0, -0.05)) {
  temp <- temp_df[temp_df$percDownloaded > i,'device']
  levels(trainSet$device)[levels(trainSet$device) %in% temp$device] <- j
  levels(validationSet$device)[levels(validationSet$device) %in% temp$device] <- j
  levels(testSet$device)[levels(testSet$device) %in% temp$device] <- j
  j <- j -1
}

temp_df <- df %>%
  group_by(device) %>%
  summarise(total = n()) %>%
  filter(!device %in% deviceGroup) %>%
  arrange(desc(total)) %>%
  select(device) %>%
  slice(1:5)
levels(trainSet$device)[!levels(trainSet$device) %in% temp_df$device & !levels(trainSet$device) %in% seq(-1,-20,-1)] <- -21
levels(validationSet$device)[!levels(validationSet$device) %in% temp_df$device & !levels(validationSet$device) %in% seq(-1,-20,-1)] <- -21
levels(testSet$device)[!levels(testSet$device) %in% temp_df$device & !levels(testSet$device) %in% seq(-1,-20,-1)] <- -21

#OS
#Agrupando
AtributosPoucoUtilizados("os")
osGroup <- unique(df_downloaded$os)

temp_df <- df_downloaded %>%
  group_by(os) %>%
  summarise(totalDownloaded = n())

temp_df <- df %>%
  group_by(os) %>%
  summarise(total = n()) %>%
  filter(os %in% osGroup) %>%
  left_join(temp_df) %>%
  mutate(percDownloaded = totalDownloaded/total) %>%
  arrange(desc(percDownloaded))

j <- -1
for (i in seq(0.95, 0, -0.05)) {
  temp <- temp_df[temp_df$percDownloaded > i,'os']
  levels(trainSet$os)[levels(trainSet$os) %in% temp$os] <- j
  levels(validationSet$os)[levels(validationSet$os) %in% temp$os] <- j
  levels(testSet$os)[levels(testSet$os) %in% temp$os] <- j
  j <- j -1
}

temp_df <- df %>%
  group_by(os) %>%
  summarise(total = n()) %>%
  filter(!os %in% osGroup) %>%
  arrange(desc(total)) %>%
  select(os) %>%
  slice(1:5)
levels(trainSet$os)[!levels(trainSet$os) %in% temp_df$os & !levels(trainSet$os) %in% seq(-1,-20,-1)] <- -21
levels(validationSet$os)[!levels(validationSet$os) %in% temp_df$os & !levels(validationSet$os) %in% seq(-1,-20,-1)] <- -21
levels(testSet$os)[!levels(testSet$os) %in% temp_df$os & !levels(testSet$os) %in% seq(-1,-20,-1)] <- -21

#Channel
#Agrupando
AtributosPoucoUtilizados("channel")
channelGroup <- unique(df_downloaded$channel)

temp_df <- df_downloaded %>%
  group_by(channel) %>%
  summarise(totalDownloaded = n())

temp_df <- df %>%
  group_by(channel) %>%
  summarise(total = n()) %>%
  filter(channel %in% channelGroup) %>%
  left_join(temp_df) %>%
  mutate(percDownloaded = totalDownloaded/total) %>%
  arrange(desc(percDownloaded))

j <- -1
for (i in seq(0.95, 0, -0.05)) {
  temp <- temp_df[temp_df$percDownloaded > i,'channel']
  levels(trainSet$channel)[levels(trainSet$channel) %in% temp$channel] <- j
  levels(validationSet$channel)[levels(validationSet$channel) %in% temp$channel] <- j
  levels(testSet$channel)[levels(testSet$channel) %in% temp$channel] <- j
  j <- j -1
}

temp_df <- df %>%
  group_by(channel) %>%
  summarise(total = n()) %>%
  filter(!channel %in% channelGroup) %>%
  arrange(desc(total)) %>%
  select(channel) %>%
  slice(1:5)
levels(trainSet$channel)[!levels(trainSet$channel) %in% temp_df$channel & !levels(trainSet$channel) %in% seq(-1,-20,-1)] <- -21
levels(validationSet$channel)[!levels(validationSet$channel) %in% temp_df$channel & !levels(validationSet$channel) %in% seq(-1,-20,-1)] <- -21
levels(testSet$channel)[!levels(testSet$channel) %in% temp_df$channel & !levels(testSet$channel) %in% seq(-1,-20,-1)] <- -21

rm(temp_df,temp,df,df_downloaded)

trainSet$click_time <- NULL
validationSet$click_time <- NULL
testSet$click_time <- NULL

#Removendo as linhas duplicadas das bases apos agrupamento
trainSet <- distinct(trainSet)
validationSet <- distinct(validationSet)
testSet <- distinct(testSet)

write.csv(trainSet,"trainSetGroup.csv", row.names = FALSE)
write.csv(validationSet,"validationSetGroup.csv", row.names = FALSE)
write.csv(testSet,"testSetGroup.csv", row.names = FALSE)