library(tidyverse)
library(kernlab)
library(caret)
library(ROCR)
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

pred_logit <- prediction(test$preds, test$class)
perf_logit <- performance(pred_logit, measure = "tpr", x.measure = "fpr")


plot(perf_logit, main = "ROC Curve for Logistic Regression Model")
abline(a=0, b = 1, lwd = 2, lty = 2)
