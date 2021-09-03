#Q1
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


#Q2
preds <- sapply(fits, function(fit){
  predict(fit, mnist_27$test[-1])
})

dim(preds)
dim(preds)[1] == length(mnist_27$test$y)
dim(preds)[2] == length(models)


#Q3
accuracies <- sapply(models, function(pred){
  cm <- confusionMatrix(as.factor(preds[, pred]), mnist_27$test$y)
  cm$overall["Accuracy"]
})

print(accuracies)
mean(accuracies)

#Harvards solution is better:
acc <- colMeans(preds == mnist_27$test$y)
acc
mean(acc)



#Q4
ensemble_pred <- ifelse(rowMeans(preds == "7") > 0.5, 7, 2)

ensemble_acc <- mean(ensemble_pred == mnist_27$test$y)

ensemble_acc


#Q5
sum(accuracies > ensemble_acc)
print(which(accuracies > ensemble_acc))



#Q6
train_accuracies <- sapply(fits, function(fit){
  min(fit$results$Accuracy)
})

mean(train_accuracies)


#Q7

good_models <- names(which(train_accuracies >= 0.8))

good_predicts <- preds[, good_models]

good_ensemble_pred <- ifelse(rowMeans(good_predicts == "7") >= 0.5, 7, 2)

good_ensemble_acc <- mean(good_ensemble_pred == mnist_27$test$y)

good_ensemble_acc

#Harvards solution is just slightly shorter in preprocessing:
ind <- train_accuracies >= 0.8
votes <- rowMeans(preds[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)
