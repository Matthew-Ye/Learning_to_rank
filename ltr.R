suppressMessages(require(randomForest)) 
library(data.table)
library(caret)
setwd('/Users/mingjie/Desktop/ltr/ranking/tensorflow_ranking/examples')
ohsumed.dt <- fread("./data/dataset.csv", sep = ",", header=TRUE)
head(ohsumed.dt)
## rename 
col.names <- paste(rep("C", 3), seq(1:3), sep = "")
col.names <- c("r", "qid", "docid", col.names)
setnames(ohsumed.dt, col.names)
## use the benefits of data.table to quicly prepare the data.
letor <- ohsumed.dt

## visualize a small fraction data
head(letor)
length(unique(letor$qid)) ## number of queries
length(unique(letor$docid)) ## unique docid




## column "r" = relevance: The larger value the relevance label has, the more
## relevant the query-docs pair. For this assignment I am going to consider 2,3 as
## relevant and 0,1 as irrelevant 

## set aside ~20% of the data for final testing
set.seed(100)
## random index
rIdx <- sample(nrow(letor), nrow(letor)*0.8)

train <- letor[rIdx, ]
test <- letor[-rIdx, ]

# Define train control for k fold cross validation
train_control <- trainControl(method="cv", number=5)
##fit the model
formula <- as.factor(r) ~ C1+C2+C3

rf.fit <- randomForest(formula, train, importance=TRUE, trControl=train_control)
varImpPlot(rf.fit)
prediction <- predict(rf.fit, test)
confusionMatrix(prediction, factor(test$r,levels = c(0,1,2,3)))



# Ranking
rankdf <- data.frame(qid = test$qid, docid = test$docid, r= test$r, pred = prediction)
rankdf <- rankdf[order(rankdf$pred, decreasing = TRUE),] 
rankdf

