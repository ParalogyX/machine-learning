set.seed(1, sample.kind = "Rounding")
library(caret)
library(tidyverse)
data("heights")

test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

ks <- seq(1, 101, 3)

x = train_set$height
y = train_set$sex
F1_scores <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(train_set$sex))
  F_meas(test_set$sex, y_hat)
  #F_meas(y_hat, test_set$sex)
})
  

plot(ks, F1_scores)
max(F1_scores)
ks[which.max(F1_scores)]


#Q2
library(dslabs)
library(caret)
data("tissue_gene_expression")

data_set <- as.data.frame(tissue_gene_expression)


set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(data_set$y, times = 1, p = 0.5, list = FALSE)
train_set <- data_set[-test_index,]
test_set <- data_set[test_index,]

ks = seq(1, 11, 2)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = train_set, k = k)
  
  y_hat <- predict(fit, train_set, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train_set$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, test_set, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test_set$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(k = k, test = test_error, train = train_error)
  
})

accuracy


#Harvards solution is a bit shorter, but actually not better then mine
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})