
# LOGISTIC REGRESSION

# Setup -------------------------------------------------------------------

library(tidyverse)
library(caret)
library(pROC)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")


# Preperation -------------------------------------------------------------

DFopt = DF[1:(0.3*nrow(DF)),]
  
DFkfold = DF[-(1:(0.3*nrow(DF))),]


# Optimisation ------------------------------------------------------------

# Pre-optimising hyperparameters with all features -------------------------

# Creating function that takes feature combination vector as input
logistic = function(comb) {
  DFtrain = DFopt[1:(0.7*nrow(DFopt)),comb]
  Xtrain = as.matrix(DFtrain[,c(1:(ncol(DFtrain)-1))])
  Ytrain <<- as.matrix(DFtrain[,ncol(DFtrain)])
  
  DFtest = DFopt[-(1:(0.7*nrow(DFopt))),comb]
  Xtest = as.matrix(DFtest[,c(1:(ncol(DFtest)-1))])
  Ytest <<- as.matrix(DFtest[,ncol(DFtest)])
  
  # Applying logistic regresion
  temp_model = glm(Y ~ ., data=DFtrain, family=binomial(link='logit'))
  
  # Extracting the betas
  beta_logistic  = temp_model$coefficients
  
  # Calculating training etas & probabilities
  eta_log_train <<- rep(0, nrow(Xtrain))
  prob_log_train <<- rep(0, nrow(Xtrain))
  for (i in 1:nrow(Xtrain)) {
    eta_log_train[i] <<- round(exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic)))
    prob_log_train[i] <<- exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic))
  }
  
  # Calculating training accuracy
  errors = 0
  for (i in 1:nrow(Xtrain)) {
    if(eta_log_train[i] != Ytrain[i]) {
      errors = errors + 1
    }
  }
  accuracy_train = 1-(errors/nrow(Xtrain))
  
  # Calculating testing etas & probabilities
  eta_log_test <<- rep(0, nrow(Xtest))
  prob_log_test <<- rep(0, nrow(Xtest))
  for (i in 1:nrow(Xtest)) {
    eta_log_test[i] <<- round(exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic)))
    prob_log_test[i] <<- exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic))
  }
  
  # Calculating testing errors
  errors = 0
  for (i in 1:nrow(Xtest)) {
    if(eta_log_test[i] != Ytest[i]) {
      errors = errors + 1
    }
  }
  accuracy_test = 1-(errors/nrow(Xtest))
  
  return(c(accuracy_train, accuracy_test))
}

# As there are no hyperparameters we chose to optimise for logistic regression, the optimal model with all features is:
logistic(c(1,10))


# Optimising features with pre-optimised hyperparameters ------------------

feature_test = matrix(nrow = 511, ncol = 3)
colnames(feature_test) = c("comb",
                           "accuracy_train",
                           "accuracy_test")

# This for-loop takes quite long to finish
k = 0
for(i in 1:9) {
  for(j in 1:ncol(combn(9,i))) {
    k = k + 1
    feature_test[k,1] = paste(rbind(combn(9,i))[,j], collapse = ",")
    feature_test[k,2:3] = logistic(c(as.double(paste(rbind(combn(9,i))[,j])),10))
  }
}
# Save(feature_test, file = "../Roeser, Jonas - 2_Data/feature_test.RData")

# Reading the feature_test created in the for-loop
load("../Roeser, Jonas - 2_Data/feature_test.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/feature_test.RData")

best_comb = feature_test[which.max(feature_test[,3]),1]
# --> We get the highest testing accuracy when training with all feateures, except head to head!


# Optimising hyperparameters with optimised features -----------------------

# Again, there are no hyperparameters we chose to optimise for logistic regression


# Plotting ROC curve ------------------------------------------------------
DFopt[,c(1:5,7:10)]

Y = DFopt$Y
model = glm(Y ~ ., data=DFopt, family=binomial(link='logit'))
ROC_model = roc(DFopt$Y, model$fitted.values)
plot(ROC_model,legacy.axes = TRUE, xlab = "false positive rate", ylab = "true positive rate", col = "blue")

#Area under the ROC
auc(ROC_model)
# = 0.7101


# Kfold -------------------------------------------------------------------

logistic_kfold = function(comb, data) {
  DFtrain = data[testIndexes,comb]
  Xtrain = as.matrix(DFtrain[,c(1:(ncol(DFtrain)-1))])
  Ytrain = as.matrix(DFtrain[,ncol(DFtrain)])
  
  DFtest = data[-testIndexes,comb]
  Xtest = as.matrix(DFtest[,c(1:(ncol(DFtest)-1))])
  Ytest = as.matrix(DFtest[,ncol(DFtest)])
  
  # Applying logistic regresion
  model = glm(Y ~ ., data=DFtrain, family=binomial(link='logit'))
  
  # Extracting the betas
  beta_logistic  = model$coefficients
  
  # Calculating training etas & probabilities
  eta_log_train = rep(0, nrow(Xtrain))
  prob_log_train = rep(0, nrow(Xtrain))
  for (i in 1:nrow(Xtrain)) {
    eta_log_train[i] = round(exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic)))
    prob_log_train[i] = exp(c(1, Xtrain[i,]) %*% beta_logistic) / (1+exp(c(1,Xtrain[i,]) %*% beta_logistic))
  }
  
  # Calculating training accuracy
  errors = 0
  for (i in 1:nrow(Xtrain)) {
    if(eta_log_train[i] != Ytrain[i]) {
      errors = errors + 1
    }
  }
  accuracy_train = 1-(errors/nrow(Xtrain))
  
  # Calculating testing etas & probabilities
  eta_log_test = rep(0, nrow(Xtest))
  prob_log_test = rep(0, nrow(Xtest))
  for (i in 1:nrow(Xtest)) {
    eta_log_test[i] = round(exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic)))
    prob_log_test[i] = exp(c(1, Xtest[i,]) %*% beta_logistic) / (1+exp(c(1,Xtest[i,]) %*% beta_logistic))
  }
  
  # Calculating testing errors
  errors = 0
  for (i in 1:nrow(Xtest)) {
    if(eta_log_test[i] != Ytest[i]) {
      errors = errors + 1
    }
  }
  accuracy_test = 1-(errors/nrow(Xtest))
  
  return(c(accuracy_train, accuracy_test))
}

# Because we are unable to handle very large amounts of data for random forests, we split DFkfold up into 3 subsets
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
  kfold[i,] = logistic_kfold(c(1,2,3,4,5,7,8,9,10), DFkfold1)
}
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes = which(folds2==i,arr.ind=TRUE)
  kfold[i+10,] = logistic_kfold(c(1,2,3,4,5,7,8,9,10), DFkfold2)
}
for(i in 1:10){
  # Segement your data by fold using the which() function
  testIndexes = which(folds3==i,arr.ind=TRUE)
  kfold[i+20,] = logistic_kfold(c(1,2,3,4,5,7,8,9,10), DFkfold3)
}

# Saving model accuracy as "model_accuracy_RF.RData"
model_accuracy_LOG = colMeans(kfold)
# save(model_accuracy_LOG, file = "../Roeser, Jonas - 2_Data/model_accuracy_LOG.RData")