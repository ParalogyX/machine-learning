library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

#Q1
n_rating <- movielens %>% 
  group_by(movieId) %>%
  summarize(n = n(), year = first(year))

n_rating %>% ggplot(aes(x = as.character(year), y = n)) + 
              geom_boxplot() + scale_y_sqrt() + 
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Harvards solution is better than mine:
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  scale_y_sqrt() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Q2
n_rating <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>% 
  summarise(n_per_year = mean(n()/(2018 - year)), title = first(title), year = as.character(first(year)))

top_number_rating <- n_rating[order(n_rating$n_per_year, decreasing = TRUE), ] %>% head(25)
  
tst <- movielens %>% group_by(movieId) %>%
  filter(movieId %in% top_number_rating$movieId) %>%
  summarise(title = first(title), av_rating = mean(rating))

merge(x = top_number_rating, y = tst, by = c("movieId", "title"))[c(1,2,4,3,5)]


#Harvards solution is beter again. I forgot about "arrange" function
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))


#Q3
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(strata_rate = round(n/years)) %>% 
  group_by(strata_rate) %>%
  summarise(av_rating = mean(rating)) %>%
  ggplot(aes(strata_rate, av_rating)) +
  geom_line() + geom_smooth()

#My answer is beter than Harvards, no stratification in their one:
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()
