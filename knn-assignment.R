set.seed(1, sample.kind = "Rounding")
library(caret)
library(tidyverse)
data("heights")

test_index <- createDataPartition(heights$height, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

ks <- seq(1, 101, 3)

x = train_set$height
y = train_set$sex
F1_scores <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = levels(train_set$sex))
  #F_meas(test_set$sex, y_hat)
  F_meas(y_hat, test_set$sex)
})
  


max(F1_scores)
ks[which.max(F1_scores)]
