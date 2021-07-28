
library(tidyverse)
library(caret)

# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()


#Q1
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
mu_1 <- seq(0, 3, len=25)

res_calc <- function(x){
  dat <- make_data(mu_1 = x)
  glm_fit <- glm(y ~ x, data=dat$train, family = "binomial")
  p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")
  y_hat_logit <- ifelse(p_hat_logit > 0.5, "1", "0") %>% factor
  as.numeric(confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]])
  
}

#res <- as.numeric(map(mu_1, res_calc)) Common! as.numeric and map? Forgot about sapply??? :)
res <- sapply(mu_1, res_calc)

qplot(mu_1, res, xlab = "delta")

#Harvards solution (not better, just for info)
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)
