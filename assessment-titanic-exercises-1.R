library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Q1
set.seed(42, sample.kind="Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index,]

nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

#Q2

set.seed(3, sample.kind="Rounding")
#Random guess
y_hat <- as.factor(sample(c(0,1), nrow(test_set), replace = T))
confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]
#or
mean(y_hat == test_set$Survived)

#Q3a
mean((train_set %>% filter(Sex == "female"))$Survived == 1)
mean((train_set %>% filter(Sex == "male"))$Survived == 1)

#Harvards solution (mine is shorter):
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "female") %>%
  pull(Survived)
train_set %>%
  group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)

#Q3b
male_survived <- mean((test_set %>% filter(Sex == "male"))$Survived == 1) > 0.5
female_survived <- mean((test_set %>% filter(Sex == "female"))$Survived == 1) > 0.5

y_hat <- as.factor(as.numeric(sapply(test_set$Sex, function(x){
  if (x == "male") result <- male_survived
  if (x == "female") result <- female_survived
  
  result
})))

confusionMatrix(y_hat, test_set$Survived)$overall["Accuracy"]


#Harvards implementation is much easier:
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy


#Q4a
mean((train_set %>% filter(Pclass == 1))$Survived == 1) > 0.5
mean((train_set %>% filter(Pclass == 2))$Survived == 1) > 0.5
mean((train_set %>% filter(Pclass == 3))$Survived == 1) > 0.5

#Or
train_set %>%
  group_by(Pclass) %>%
  summarize(Survived = mean(Survived == 1))

#Q4b
class_model <- ifelse(test_set$Pclass == 1, 1, 0)   #predict Survived=1 is first class, else 0
mean(class_model == test_set$Survived)    # calculate accuracy


#Q4c
train_set %>%
  group_by(Sex, Pclass) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Survived > 0.5)

#Q4d
sex_class_model <- ifelse(test_set$Sex == "female" & (test_set$Pclass == 1 | test_set$Pclass == 2), 1, 0)
mean(sex_class_model == test_set$Survived)    # calculate accuracy

#Q5a
sex_model_result <- confusionMatrix(as.factor(sex_model), test_set$Survived)
class_model_result <- confusionMatrix(as.factor(class_model), test_set$Survived)
sex_class_model_result <- confusionMatrix(as.factor(sex_class_model), test_set$Survived)

sex_model_result$byClass["Sensitivity"] 
class_model_result$byClass["Sensitivity"] 
sex_class_model_result$byClass["Sensitivity"] 

sex_model_result$byClass["Specificity"] 
class_model_result$byClass["Specificity"] 
sex_class_model_result$byClass["Specificity"] 

sex_model_result$byClass["Balanced Accuracy"] 
class_model_result$byClass["Balanced Accuracy"] 
sex_class_model_result$byClass["Balanced Accuracy"] 


#Q5a
max(sex_model_result$byClass["Balanced Accuracy"] , class_model_result$byClass["Balanced Accuracy"] ,sex_class_model_result$byClass["Balanced Accuracy"])


#Q6a
#first w/o F_meas(), just from confMatrix:
sex_model_result$byClass["F1"] 
class_model_result$byClass["F1"] 
sex_class_model_result$byClass["F1"] 

F_meas(as.factor(sex_model), test_set$Survived)
F_meas(as.factor(class_model), test_set$Survived)
F_meas(as.factor(sex_class_model), test_set$Survived)


