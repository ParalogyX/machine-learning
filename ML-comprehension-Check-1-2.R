library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7
# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]


#Q8
min(test$Sepal.Length)
max(test$Sepal.Length)
min(test$Sepal.Width)
max(test$Sepal.Width)
min(test$Petal.Length)
max(test$Petal.Length)
min(test$Petal.Width)
max(test$Petal.Width)


cutoff <- seq(1, 8, 0.1)
accuracy_s_l <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
accuracy_s_w <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
accuracy_p_l <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
accuracy_p_w <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})


data.frame(cutoff, accuracy_s_l, accuracy_s_w, accuracy_p_l, accuracy_p_w) %>% 
  ggplot(aes(cutoff)) + 
  geom_line(aes(y = accuracy_s_l), colour = 'red') + 
  geom_line(aes(y = accuracy_s_w), colour = 'blue') +
  geom_line(aes(y = accuracy_p_l), colour = 'green') + 
  geom_line(aes(y = accuracy_p_w), colour = 'pink')
max(accuracy_s_l)
max(accuracy_s_w)
max(accuracy_p_l)
max(accuracy_p_w)
max(max(accuracy_s_l), max(accuracy_s_w), max(accuracy_p_l), max(accuracy_p_w))

#My solution is too complicated, Harvards solution is better:
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


#Q9
cutoff <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(y))
  mean(y_hat == train$Species)
})
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(y))
mean(y_hat == test$Species)

#Harvards solution based on #Q8
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

#Q10

foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,max)	

#Q11
plot(iris,pch=21,bg=iris$Species)

#optimize cutoff for Petal.Length
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs_lenght <-rangedValues[which(predictions==max(predictions))]
best_cutoff_lenght <- cutoffs_lenght[1]

#optimize cutoff for Petal.Width
predictions <- foo(train[,4])
rangedValues <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs_width <-rangedValues[which(predictions==max(predictions))]
best_cutoff_width <- cutoffs_width[1]


y_hat <- ifelse(test[,3]>best_cutoff_lenght | test[,4]>best_cutoff_width, 'virginica',  'versicolor')
mean(y_hat==test$Species)














