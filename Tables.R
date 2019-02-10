#install.packages('xtable')
library(tidyverse)
library(xtable)
library(dplyr)

rm(list = ls())

features <- c("rank", "rank move", "form", "head to head", "condition wins", "home town", "age difference", "height difference", "fatigue")
selectedfeatures <- data.frame(features)
colnames(selectedfeatures) = "Features"
xtable(selectedfeatures)

ModelAccuracyLog <- as.data.frame(t(model_accuracy_LOG))
colnames(ModelAccuracyLog) <- c("Training Accuracy", "Logistic Regression")
xtable(ModelAccuracyLog, digits = 5)

ModelAccuracyRFBag <- as.data.frame(t(model_accuracy_RF))
colnames(ModelAccuracyRFBag) <- c("Training Accuracy", "Random Forest Bagging")
xtable(ModelAccuracyRFBag, digits = 5)

ModelAccuracyRFBoost <- as.data.frame(model_accuracy_bRF)
colnames(ModelAccuracyRFBoost) <- c("Random Forest Boosted")
xtable(ModelAccuracyRFBoost, digits = 5)

ModelAccuracyNN <- as.data.frame(model_accuracy_NN)
colnames(ModelAccuracyNN) <- c("Neural Network")
xtable(ModelAccuracyNN, digits = 5)

ModelAccuracyTestLog <- as.data.frame(ModelAccuracyLog$`Logistic Regression`)
colnames(ModelAccuracyTestLog) <- c("Logistic Regression")
ModelAccuracyTestRFBag <- as.data.frame(ModelAccuracyRFBag$`Random Forest Bagging`)
colnames(ModelAccuracyTestRFBag) <- c("Random Forest Bagging")
ModelAccuracyTestRFBoo <- as.data.frame(ModelAccuracyRFBoost)
ModelAccuracyTestNN <- as.data.frame(ModelAccuracyNN)

ModelAccuracies <- cbind(ModelAccuracyTestRFBag, ModelAccuracyTestRFBoo, ModelAccuracyTestLog, ModelAccuracyTestNN)
rownames(ModelAccuracies) <- c("Testing Accuracy")

xtable(ModelAccuracies, digits = 5)
