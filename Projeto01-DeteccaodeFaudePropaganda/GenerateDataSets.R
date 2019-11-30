library(data.table)
library(caret)

df <- fread("train_sample.csv", colClasses=c('NULL', rep('factor', 4), 'character', 'NULL', 'factor'))
distDownloadedDois <- nrow(df[df$is_attributed == 1,])/nrow(df)

#Separando apenas a classe 1
df_downloaded <- filter(df, is_attributed == 1)

trainIndex <- createDataPartition(df_downloaded$is_attributed, p = .8, list = FALSE, times = 1)
trainSetAll <- df_downloaded[trainIndex,]
validationIndex <- createDataPartition(trainSetAll$is_attributed, p = .7, list = FALSE, times = 1)

trainSet <- trainSetAll[validationIndex,]
validationSet <- trainSetAll[-validationIndex,]
testSet  <- df_downloaded[-trainIndex,]
rm(df_downloaded)

#Separando a classe 0 e dando um rbind com sample
df_notdownloaded <- filter(df, is_attributed == 0)
rm(df)

rowsSet <- sample.int(length(df_notdownloaded$is_attributed),size=length(trainSet$is_attributed), replace=FALSE)
trainSet <- rbind(trainSet,df_notdownloaded[rowsSet,])
df_notdownloaded <- df_notdownloaded[-rowsSet,]
trainSet <- trainSet[sample(nrow(trainSet)),]
write.csv(trainSet,"trainSet.csv", row.names = FALSE)

rowsSet <- sample.int(length(df_notdownloaded$is_attributed),size=length(validationSet$is_attributed), replace=FALSE)
validationSet <- rbind(validationSet,df_notdownloaded[rowsSet,])
df_notdownloaded <- df_notdownloaded[-rowsSet,]
validationSet <- validationSet[sample(nrow(validationSet)),]
write.csv(validationSet,"validationSet.csv", row.names = FALSE)

rm(trainSet)
rm(validationSet)

rowsSet <- sample.int(length(df_notdownloaded$is_attributed),size=round(length(testSet$is_attributed)/distDownloadedDois), replace=FALSE)
testSet <- rbind(testSet,df_notdownloaded[rowsSet,])

rm(df_notdownloaded)
testSet <- testSet[sample(nrow(testSet)),]
write.csv(testSet,"testSet.csv", row.names = FALSE)