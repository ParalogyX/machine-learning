library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


#Q1
dat_inclass <- dat %>% filter(type == "inclass")
mean(dat_inclass$sex == 'Female')

dat_online <- dat %>% filter(type == "online")
mean(dat_online$sex == 'Female')

#Harvards solution
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))



#Q2
y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y))
mean(y_hat == dat$sex)


#Q3
table(y_hat, y)

#Q4
sensitivity(y_hat,y)

#Q5
specificity(y_hat,y)

#Q6
mean(dat$sex == "Female")
#or
mean(y == "Female")


