#install.packages("readr")
#install.packages("party")
#install.packages("iterators")
#install.packages("scales")
#install.packages("pbkrtest")
#install.packages("lme4")
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
#install.packages(packageurl, repos=NULL, type="source")
#install.packages("caret", dependencies = TRUE)
#install.packages("qdap")

rm(list = ls(all = TRUE))
setwd("C:/Users/Ofir/Desktop/ex4")
library(readr)
unzip("train.csv.zip")
train <- read_csv("train.csv")
unzip("test.csv.zip")
test  <- read_csv("test.csv")
train$median_relevance <- factor(train$median_relevance)

require(stringdist)
train$stringDistance = stringdist(train$query, train$product_description, method="lv")
test$stringDistance = stringdist(test$query, test$product_description, method="lv")

levels(train$query) <- union(levels(train$query), levels(test$query))
levels(train$product_title) <- union(levels(train$product_title), levels(test$product_title))
levels(train$product_description) <- union(levels(train$product_description), levels(test$product_description))
levels(train$sen) <- union(levels(train$sen), levels(test$sen))
levels(train$stringDistance) <- union(levels(train$stringDistance), levels(test$sen))

levels(test$query) <- union(levels(train$query), levels(test$query))
levels(test$product_title) <- union(levels(train$product_title), levels(test$product_title))
levels(test$product_description) <- union(levels(train$product_description), levels(test$product_description))
levels(test$sen) <- union(levels(train$sen), levels(test$sen))
levels(test$stringDistance) <- union(levels(train$stringDistance), levels(test$stringDistance))
inTraining <- sample(1:nrow(train),  .75*nrow(train))
training <- train[ inTraining,]

testing  <- train[-inTraining,]

library(randomForest)
model1 <- randomForest(median_relevance ~ train$query+train$product_title+train$product_description, data=training, ntree=4)

library(party)
library(caret)
model2 <- train(median_relevance ~ query+product_title+product_description, data = training,
                method = "rpart",
                trControl = trainControl(classProbs = F))
names(getModelInfo())

results1 <- predict(model1, newdata = testing)
results2 <- predict(model2, newdata = testing)

#install.packages("Metrics")
library(Metrics)
ScoreQuadraticWeightedKappa(testing$median_relevance, results1, 1, 4)
ScoreQuadraticWeightedKappa(testing$median_relevance, results2, 1, 4)