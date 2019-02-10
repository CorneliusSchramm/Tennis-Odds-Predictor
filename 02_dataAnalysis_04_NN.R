
# RANDOM FORESTS WITH BAGGING OPTIMIZATION

# Setup -------------------------

library(tidyverse)
library(caret)
library(pROC)
# cran <- getOption("repos")
# cran["dmlc"] <- "https://s3-us-west-2.amazonaws.com/apache-mxnet/R/CRAN/"
# options(repos = cran)
# install.packages("mxnet",dependencies = T)
library(mxnet)

rm(list = ls())

# Reading the previously saved version of our data
load("../Roeser, Jonas - 2_Data/DF.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/DF.RData")


# Preperation ------------

# Data Formatting
DFopt = DF[1:(0.3*nrow(DF)),]
DFkfold = DF[-(1:(0.3*nrow(DF))),]

# Train() needs the ouput to be a factor of two levels
DFopt[,ncol(DFopt)] = as.factor(DFopt[,ncol(DFopt)])
DFkfold[,ncol(DFkfold)] = as.factor(DFkfold[,ncol(DFopt)])


# Optimisation --------------------------------------------------

# Pre-optimising hyperparameters with all features ---------------

# The grid allows us to create several different models with various parameter settings.
# Grid features
grid = expand.grid(layer1 = seq(1,11,2),
                   layer2 = seq(0,8,2),
                   layer3 = seq(0,9,3),
                   learning.rate = c(0.05,0.1),
                   momentum = 0.85,
                   dropout = 0.5,
                   activation = "relu"
)

# We are using 5-fold cross-validation for paramter tuning
control = trainControl(method = "cv",
                       number = 5)

temp_model = train(Y ~ .,
                   data = DFopt,
                   method = "mxnet",
                   tuneGrid = grid,
                   trControl = control,
                   num.round = 10,
                   maximize = TRUE
                   )

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were
# layer1 = 5
# layer2 = 6
# layer3 = 6
# learning.rate = 0.1
# momentum = 0.85
# dropout = 0.5
# activation = "relu"


# Optimising features with pre-optimized hyperparameters ----------

# Reading the feature_test created with logistic regression
load("../Roeser, Jonas - 2_Data/feature_test.RData")

# Because of OneDrive we need to load from two different paths
load("../2_Data/feature_test.RData")

# Creating a matrix for testing the 10 best feature combinations of logistic regression
feature_test_nn = matrix(nrow = 10, ncol = 2)
colnames(feature_test_oob) = c("comb",
                               "nn_accuracy")
feature_test_nn[,1] = feature_test[order(feature_test[,3], decreasing=T)[1:10],1]

# Creating function that takes feature combination vector as input
neural = function(comb) {
  DFopt = DFopt[,comb]
  
  grid = expand.grid(layer1 = 5,
                     layer2 = 6,
                     layer3 = 6,
                     learning.rate = 0.1,
                     momentum = 0.85,
                     dropout = 0.5,
                     activation = "relu"
  )
  
  # We are using 5-fold cross-validation for paramter tuning
  control = trainControl(method = "cv",
                         number = 10)
  
  temp_model = train(Y ~ .,
                     data = DFopt,
                     method = "mxnet",
                     tuneGrid = grid,
                     trControl = control,
                     num.round = 10,
                     maximize = TRUE
  )
  
  return(temp_model$results$Accuracy)
}

for(i in 1:10) {
  feature_test_nn[i,2] = neural(c(unlist(lapply(strsplit(feature_test_nn[i,1], split=","), as.numeric)),10))
}

best_comb = feature_test_nn[which.max(feature_test_nn[,2]),1]
# --> We get the highest testing accuracy when training with all feateures, except home game!

# Optimising hyperparameters with optimized features --------------

# Now we use the optimal feature combination from above
best_comb

# The grid allows us to create several different models with various parameter settings.
# Grid features
grid = expand.grid(layer1 = c(4,5,6),
                   layer2 = c(5,6,7),
                   layer3 = c(5,6,7),
                   learning.rate = c(0.075,0.125),
                   momentum = 0.85,
                   dropout = 0.5,
                   activation = "relu"
)

# We are using 5-fold cross-validation for paramter tuning
control = trainControl(method = "cv",
                       number = 5)

temp_model = train(Y ~ .,
              data = DFopt[,c(1,2,3,4,5,6,7,8,10)],
              method = "mxnet",
              tuneGrid = grid,
              trControl = control,
              num.round = 20,
              maximize = TRUE
)

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were
# layer1 = 6
# layer2 = 5
# layer3 = 6
# learning.rate = 0.075
# momentum = 0.85
# dropout = 0.5
# activation = "relu"

# Creating the optimal model
grid = expand.grid(layer1 = 6,
                   layer2 = 5,
                   layer3 = 6,
                   learning.rate = 0.075,
                   momentum = 0.85,
                   dropout = 0.5,
                   activation = "relu"
)

control = trainControl(method = "cv",
                       number = 5)

model = train(Y ~ .,
                   data = DFopt[,c(1,2,3,4,5,6,7,8,10)],
                   method = "mxnet",
                   tuneGrid = grid,
                   trControl = control,
                   num.round = 20,
                   maximize = TRUE
)


# Plotting ROC curve -------------------

NN_probs <- predict(model, DFopt, type = "prob")

pred_NN <- prediction(NN_probs[,1], DFopt$Y)
perf_NN <- performance(pred_NN, "fpr", "tpr")
plot(perf_NN,xlim = c(0, 1), ylim = c(0, 1), type = "l", 
     xlab = "false positive rate", ylab = "true positive rate", col = 'green')
abline(0, 1, col= "black")

#AUC
auc_NN = performance(pred_NN, "auc")
auc_ROCR = 1-auc_NN@y.values[[1]]
#0.7157735

# Kfold --------------------------------

# Perform 10 fold cross validation
neural_kfold = function (data,comb) {
  data = data[,comb]
  
  grid = expand.grid(layer1 = 6,
                     layer2 = 5,
                     layer3 = 6,
                     learning.rate = 0.075,
                     momentum = 0.85,
                     dropout = 0.5,
                     activation = "relu"
                     )

  control = trainControl(method = "cv",
                       number = 10)

  model = train(Y ~ .,
                data = data,
                method = "mxnet",
                tuneGrid = grid,
                trControl = control,
                num.round = 20,
                maximize = TRUE
                )
  
  return(model$results$Accuracy)
}

# Because we are unable to handle very large amounts of data, we split DFkfold up into 3 subsets
DFkfold1 = DFkfold[1:(nrow(DFkfold)/3),]
DFkfold2 = DFkfold[(nrow(DFkfold)/3):(nrow(DFkfold)*2/3),]
DFkfold3 = DFkfold[(nrow(DFkfold)*2/3):nrow(DFkfold),]

# Create kfold matrix
kfold = matrix(nrow = 3, ncol = 1)

best_comb

# Perform 10 fold cross validation for each DKfold
kfold[1,1] = neural_kfold(DFkfold1, c(1,2,3,4,5,6,7,8,10))
kfold[2,1] = neural_kfold(DFkfold2, c(1,2,3,4,5,6,7,8,10))
kfold[3,1] = neural_kfold(DFkfold3, c(1,2,3,4,5,6,7,8,10))

# Saving model accuracy as "model_accuracy_NN.RData"
model_accuracy_NN = colMeans(kfold)
# save(model_accuracy_NN, file = "../Roeser, Jonas - 2_Data/model_accuracy_NN.RData")