
# RANDOM FORESTS WITH BAGGING OPTIMIZATION

# Setup -------------------------

library(tidyverse)
library(ggplot2)
library(caret)
library(cowplot)
library(randomForest)
library(pROC)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")


# Preperation ------------

# So we dont get an error due to too big of an vector 
memory.limit(45000)

# Data Formatting
DFopt = DF[1:(0.3*nrow(DF)),]
DFkfold = DF[-(1:(0.3*nrow(DF))),]

DFopt$Y = as.factor(DFopt$Y) #so the random forest function will treat it as a classification problem
DFkfold$Y = as.factor(DFkfold$Y)


# Optimisation --------------------------------------------------

# Pre-optimising hyperparameters with all features ---------------

# Finding the necessary number of trees
temp_model = randomForest(Y ~ ., data=DFopt, ntree=1000)
oob.error.data = data.frame(
  Trees=rep(1:nrow(temp_model$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(temp_model$err.rate)),
  Error=temp_model$err.rate[,"OOB"])

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  theme_bw() +
  geom_line(aes(color=Type))
# We can see that the OOB error rate does not decrease significantly after 500.
# It is therefore sufficient to proceed with the standart 500 trees

# Finding the optimal number of variables at each internal node
oob_values = vector(length=10)

for(i in 1:10) {
  temp_model = randomForest(Y ~ ., data=DFopt, mtry=i)
  oob_values[i] = temp_model$err.rate[nrow(temp_model$err.rate),1]
}
oob_values
# We can see that mtry = 1 has the lowest OOB error rate and is therefore optimal


# Optimising features with pre-optimized hyperparameters ----------

# Reading the feature_test created with logistic regression
load("../Roeser, Jonas - 2_Data/feature_test.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/feature_test.RData")

# Creating a matrix for testing the 10 best feature combinations of logistic regression
feature_test_oob = matrix(nrow = 10, ncol = 2)
colnames(feature_test_oob) = c("comb",
                           "oob_error")
feature_test_oob[,1] = feature_test[order(feature_test[,3], decreasing=T)[1:10],1]

# Creating function that takes feature combination vector as input
forest = function(comb) {
  DFopt = DFopt[,comb]
  
  # Applying random forest
  temp_model = randomForest(Y ~ ., data=DFopt, mtry=1)
  
  return(temp_model$err.rate[nrow(temp_model$err.rate),1])
}

for(i in 1:10) {
  feature_test_oob[i,2] = forest(c(unlist(lapply(strsplit(feature_test_oob[i,1], split=","), as.numeric)),10))
}

best_comb = feature_test_oob[which.min(feature_test_oob[,2]),1]
# --> We get the highest testing accuracy when training with all feateures, except fatigue!


# Optimising hyperparameters with optimized features --------------

# Now we use the optimal feature combination from above
best_comb

# Finding the necessary number of trees
temp_model = randomForest(Y ~ ., data=DFopt[,c(1:7,9,10)], ntree=4000)
oob.error.data = data.frame(
  Trees=rep(1:nrow(temp_model$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(temp_model$err.rate)),
  Error=temp_model$err.rate[,"OOB"])

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  theme_bw() +
  geom_line(aes(color=Type))
# We can see that the OOB error rate does not decrease significantly after 2000.
# It is therefore sufficient to proceed with 2000 trees


# Finding the optimal number of variables at each internal node
oob_values = vector(length=10)

for(i in 1:10) {
  temp_model = randomForest(Y ~ ., data=DFopt[,c(1,2,3,4,5,6,7,9,10)], mtry=i, ntree=2000)
  oob_values[i] = temp_model$err.rate[nrow(temp_model$err.rate),1]
}
oob_values
# We can see that mtry = 1 still has the lowest OOB error rate and is therefore optimal

# Plotting ROC curve -------------------

# Creating the optimal model
model = randomForest(Y ~ ., data=DFopt[,c(1:7,9,10)], mtry=1, ntree=2000)
DFopt[,c(1:7,9,10)]
ROC_model = roc(DFopt$Y,model$votes[,2])
plot(ROC_model,legacy.axes = TRUE,xlab = "false positive rate", ylab = "true positive rate", col = "red")
#Area under the ROC
auc(ROC_model)
# = 0.7085


# Kfold --------------------------------

# Perform 10 fold cross validation
forest_kfold = function(comb, data, folds) {
  # Only choose the applicable feature combination
  data = data[,comb]
  
  # Segement your data by fold using the which() function
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- data[testIndexes, ]
  trainData <- data[-testIndexes, ]
  
  # So randomeForest will do a classification, not a regression:
  trainData$Y = as.factor(trainData$Y)
  testData$Y = as.factor(testData$Y)
  
  # Splitting X and Y
  Ytrain = as.data.frame(trainData[,ncol(trainData)])            
  Ytest = as.data.frame(testData[,ncol(testData)])
  
  # Training model
  model = randomForest(Y ~ ., data=trainData, mtry=1, ntree=2000, proximity=TRUE) #######################
  
  # Calculating the training eta
  eta_RF_train = as.data.frame(predict(model,type="response",
                                       norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE))
  
  # Calculating training accuracy
  errors = 0
  for (i in 1:nrow(trainData)) {
    if(eta_RF_train[i,1] != Ytrain[i,1]) {
      errors = errors + 1
    }
  }
  accuracy_train = 1-(errors/nrow(Ytrain))
  
  # Calculating the testing eta
  eta_RF_test = as.data.frame(predict(model,testData, type="response",
                                      norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE))
  
  # Calculating testing errors
  errors = 0
  for (i in 1:nrow(testData)) {
    if(eta_RF_test[i,1] != Ytest[i,1]) {
      errors = errors + 1
    }
  }
  accuracy_test = 1-(errors/nrow(Ytest))
  
  return(c(accuracy_train, accuracy_test))
}

# Because we are unable to handle very large amounts of data, we split DFkfold up into 3 subsets
DFkfold1 = DFkfold[1:(nrow(DFkfold)/3),]
DFkfold2 = DFkfold[(nrow(DFkfold)/3):(nrow(DFkfold)*2/3),]
DFkfold3 = DFkfold[(nrow(DFkfold)*2/3):nrow(DFkfold),]

# Create 10 equally sized folds for each DKfold
folds1 = cut(seq(1,nrow(DFkfold1)),breaks=10,labels=FALSE)
folds2 = cut(seq(1,nrow(DFkfold2)),breaks=10,labels=FALSE)
folds3 = cut(seq(1,nrow(DFkfold3)),breaks=10,labels=FALSE)

# Create kfold matrix
kfold = matrix(nrow = 30, ncol = 2)

best_comb

# Perform 10 fold cross validation for each DKfold
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes = which(folds1==i,arr.ind=TRUE)
  kfold[i,] = forest_kfold(c(1,2,3,4,5,6,7,9,10), DFkfold1, folds1)
}
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes = which(folds2==i,arr.ind=TRUE)
  kfold[i+10,] = forest_kfold(c(1,2,3,4,5,6,7,9,10), DFkfold2, folds2)
}
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes = which(folds3==i,arr.ind=TRUE)
  kfold[i+20,] = forest_kfold(c(1,2,3,4,5,6,7,9,10), DFkfold3, folds3)
}

# Saving model accuracy as "model_accuracy_RF.RData"
model_accuracy_RF = colMeans(kfold)
save(model_accuracy_RF, file = "../Roeser, Jonas - 2_Data/model_accuracy_RF.RData")