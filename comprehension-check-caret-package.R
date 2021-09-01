library(tidyverse)
library(dslabs)
library(caret)
library(rpart)

#Q1
data(tissue_gene_expression)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

modelLookup("rpart")
set.seed(1991, sample.kind="Rounding")

fit_rpart <- train(x, y, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))

ggplot(fit_rpart, highlight = TRUE)

fit_rpart$bestTune
confusionMatrix(predict(fit_rpart, tissue_gene_expression$x, type = "raw"),
                tissue_gene_expression$y)$overall["Accuracy"]

fit$bestTune
fit$results$Accuracy[as.numeric(rownames(fit$bestTune)[1])]

#Harvards solution:
fit <- with(tissue_gene_expression, 
        train(x, y, method = "rpart",
        tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)


confusionMatrix(predict(fit, tissue_gene_expression$x, type = "raw"),
                tissue_gene_expression$y)$overall["Accuracy"]

#Q2


set.seed(1991, sample.kind="Rounding")

fit_rpart <- train(x, y, method = "rpart", control = rpart.control(minsplit = 0), tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
ggplot(fit_rpart, highlight = TRUE)

fit_rpart$bestTune
confusionMatrix(predict(fit_rpart, tissue_gene_expression$x, type = "raw"),
                tissue_gene_expression$y)$overall["Accuracy"]

set.seed(1991, sample.kind="Rounding")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart", control = rpart.control(minsplit = 0),
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)
ggplot(fit, highlight = TRUE)

fit_rpart$bestTune
confusionMatrix(predict(fit_rpart, x, y, type = "raw"))$overall["Accuracy"]



#Correct answer from HarvardX:
set.seed(1991, sample.kind = "Rounding")
fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)


#Q3
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)


#Q4
modelLookup("rf")
set.seed(1991, sample.kind = "Rounding")
fit <- with(tissue_gene_expression, 
          train(x, y, method = "rf",
          tuneGrid = data.frame(mtry = seq(50, 200, 25)),
          nodesize = 1))

fit$bestTune
ggplot(fit)

#Q5
imp <- varImp(fit)
imp


#Q6
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
