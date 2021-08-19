library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

#Q1
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

#Q2
s<-sapply(indexes, function(k){
  sum(k == 3)
})

sum(s)


#Q3
y <- rnorm(100, 0, 1)
qnorm(0.75)
quantile(y, 0.75)

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
B <- 10000
R <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(R)
sd(R)

#Q4
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")

indexes <- createResample(y, 10)

q_75 <-sapply(indexes, function(k){
  quantile(y[k], 0.75)
})

mean(q_75)
sd(q_75)


#Q5
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")

indexes <- createResample(y, 10000)

q_75 <-sapply(indexes, function(k){
  quantile(y[k], 0.75)
})

mean(q_75)
sd(q_75)





