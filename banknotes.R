library(tidyverse)
library(kernlab)
library(caret)
banknotes <- read.csv("https://raw.githubusercontent.com/Saswat7101/Bank-Note-Authentication/refs/heads/main/BankNote_Authentication.csv", header = T)

banknotes$class <- as.factor(banknotes$class)
samp <- sample(nrow(banknotes), floor(nrow(banknotes) * 0.8))
train <- banknotes[samp,]
test <- banknotes[-samp,]

logit <- glm(class ~ ., data = train, family = "binomial")
test$preds <- predict(logit, test, type = "response")

test$countpred <- as.factor(ifelse(test$preds > 0.5, 1, 0))
confusionMatrix(test$countpred, test$class)

train_svm <- banknotes[samp,]
test_svm <- banknotes[-samp,]
svm <- ksvm(class ~ ., data = train_svm)
test_svm$svmpred <- predict(svm, test_svm)

confusionMatrix(test_svm$class, test_svm$svmpred)
