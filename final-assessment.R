options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#Q1
#dimentions:
dim(brca$x)
#how many samples
length(brca$y)
#how many predictors
length(brca$x[1,])
#proportion of malignant
mean(brca$y == "M")
#column number with highest mean
which.max(colMeans(brca$x))
#column number with lowest sd
which.min(colSds(brca$x))

#Q2
#scaling
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

#sd of the 1 column
sd(x_scaled[,1])
#median value of the 1 column
median(x_scaled[,1])

#Q3
d <- dist(x_scaled)
#average distance between the first sample, which is benign, and other benign samples
benign_index <- which(brca$y == "B")[-1]
mean(as.matrix(d)[1,benign_index])

#average distance between the first sample and malignant samples
malignant_index <- which(brca$y == "M")
mean(as.matrix(d)[1,malignant_index])

#Harvard's solution is different, but not better:
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)



#Q4

heatmap(cor(x_scaled), labRow = NA, labCol = NA)

#My reply is not completely correct. Harvard's solution is good:

d_features <- dist(t(x_scaled))   #I forgot to transpone matrix. 
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

#To work with samples use dist(x_scaled), but to work with features use dist(t(x_scaled))

#Q5
h <- hclust(d_features)
plot(h, cex = 0.65, main = "", xlab = "")


groups <- cutree(h, k = 5)

#Harvard's answer is a bit more convinient to read:
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

#Q6
pca <- prcomp(x_scaled)
#What proportion of variance is explained by the first principal component?
summary(pca)$importance[2,1]
#How many principal components are required to explain at least 90% of the variance?
sum(summary(pca)$importance[3,] < 0.9) + 1

#Q7
data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], tumor = as.factor(brca$y)) %>%
  ggplot(aes(PC1, PC2, color = tumor)) + geom_point()

#Harvard made dataframe a bit easier:
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()


#Q8
for(i in 1:10){
  boxplot(pca$x[,i] ~ brca$y, main = paste("PC", i))
}

#another solution, better in this case (only two outcaomes):
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()




#Part 3
#Set the seed to 1, then create a data partition splitting brca$y and the scaled version of the brca$x matrix into 
#a 20% test set and 80% train using the following code:

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

#Q9
#proportion of the training set is benign
mean(train_y == "B")

#proportion of the test set is benign
mean(test_y == "B")

#Q10a
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

set.seed(3, sample.kind = "Rounding")

k <- kmeans(train_x, centers = 2)
y_test_kmeans <- as.factor(predict_kmeans(test_x, k))
levels(y_test_kmeans) <- c("B", "M")

#overall accuracy
mean(test_y == y_test_kmeans)


#Harvards solution is the same (a bit easier because of ifelse)
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

#Why accuracy on train set is lower than on test set????

#Q10b
b_indexes_in_test <- which(test_y == "B")
mean(y_test_kmeans[b_indexes_in_test] == test_y[b_indexes_in_test])

m_indexes_in_test <- which(test_y == "M")
mean(y_test_kmeans[m_indexes_in_test] == test_y[m_indexes_in_test])

#Harvard's solution is much better, I forgot about sensitivity function:
sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")


#Q11
set.seed(1, sample.kind = "Rounding")

lm_fit <- train(train_x, train_y, method = "glm") 
confusionMatrix(predict(lm_fit, test_x, type = "raw"),
                test_y)$overall["Accuracy"]

#the same, but w/o conf matrix is by Harvard:
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

#Q12
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)

#Q13
set.seed(5, sample.kind = "Rounding")
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)


#Q14
set.seed(7, sample.kind = "Rounding")

train_knn <- train(train_x, train_y, method = "knn",
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
train_knn$bestTune
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)


#Q15a
set.seed(9, sample.kind = "Rounding")

train_rf <- train(train_x, train_y, method = "rf",
      tuneGrid = data.frame(mtry = c(3, 5, 7, 9)), importance = TRUE)
#value of mtry gives the highest accuracy
train_rf$bestTune

rf_preds <- predict(train_rf, test_x)
#accuracy of the random forest model on the test set
mean(rf_preds == test_y)

#he most important variable in the random forest model
train_rf$finalModel$importance[order(train_rf$finalModel$importance[,4], decreasing = T), ]
#Last question could be solved much easier (and this is more correct than guessing by different metrics):
varImp(train_rf)


#Q15b
row.names(varImp(train_rf)$importance)

#Q16a


#models list
all_preds <- matrix(c(y_test_kmeans, glm_preds, lda_preds, qda_preds, loess_preds, knn_preds, rf_preds), ncol = 7)

ensemble_pred <- ifelse(rowMeans(all_preds == "M") > 0.5, "M", "B")
mean(ensemble_pred == test_y)

#Harvard's solution is a bit different, but not better than mine:
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

#16b

tibble(model = c("Logistic regression", "LDA", "QDA", "Loess", "Random Forest", "Knn", "K-means", "Ensemble"), 
       accuracy = c(mean(glm_preds == test_y), mean(lda_preds == test_y), mean(qda_preds == test_y), 
                    mean(loess_preds == test_y), mean(rf_preds == test_y), mean(knn_preds == test_y),
                    mean(kmeans_preds == test_y), mean(ensemble_preds == test_y))) %>% arrange(desc(accuracy))
