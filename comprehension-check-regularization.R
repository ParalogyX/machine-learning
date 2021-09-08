library(tidyverse)

options(digits=7)
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
schools %>% arrange(desc(score)) %>% select(id, size, score) %>% .[c(1, 10),]
#harvards answer is almost the same:
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

#Q2
median(schools$size)
median(schools %>% top_n(10, score) %>% .$size)
#or
schools %>% top_n(10, score) %>% .$size %>% median()

#Q3
schools %>% top_n(10, desc(score)) %>% .$size %>% median()
#or (good solution from Harvard):
schools %>% top_n(-10, score) %>% .$size %>% median()

#Q4
#I will use the new way to highlight the data (old way is to create another plot on top)
library(gghighlight)
top_10_quality <- schools %>% top_n(10, quality) %>% .$quality
schools %>% ggplot(aes(size, score)) +
  geom_point() + gghighlight(quality %in% top_10_quality)

#harvards solution is also good, but "old fashioned", without gghighlight()
#Of course, better was to use rank <=10 then top_10_quality
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

#my version with rank, also not perfect, as only 7 schools in rank <= 10:
schools %>% ggplot(aes(size, score)) +
  geom_point() + gghighlight(rank<=10)


#Q5
overall <- mean(sapply(scores, mean))

alpha = 25



schools %>% mutate(reg_score = sapply(scores, function(x){
  overall + sum(x - overall)/(length(x) + alpha)
})) %>% top_n(10, reg_score) %>% arrange(desc(reg_score))

#overall + sum(x - overall)/(length(x) + alpha) - only one correct option. Don't forget to add overall to average
schools %>% mutate(reg_score = sapply(scores, function(x){
  overall + sum(x - overall)/(length(x) + alpha)
})) %>% ggplot(aes(size, reg_score)) +
  geom_point() + gghighlight(rank<=10)

#Q6
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

alphas <- seq(10, 250)

rmses <- sapply(alphas, function(alpha){
  reg_score = sapply(scores, function(x){
    overall + sum(x - overall)/(length(x) + alpha)
  }) 
  return(RMSE(schools$quality, reg_score))
})

qplot(alphas, rmses)  
alphas[which.min(rmses)]
min(rmses)


#Q7
overall <- mean(sapply(scores, mean))

alpha = alphas[which.min(rmses)]



schools %>% mutate(reg_score = sapply(scores, function(x){
  overall + sum(x - overall)/(length(x) + alpha)
})) %>% top_n(10, reg_score) %>% arrange(desc(reg_score))



#Q8
alphas <- seq(10, 250)

rmses <- sapply(alphas, function(alpha){
  reg_score = sapply(scores, function(x){
    sum(x)/(length(x) + alpha)
  }) 
  return(RMSE(schools$quality, reg_score))
})

qplot(alphas, rmses)  
alphas[which.min(rmses)]
min(rmses)

