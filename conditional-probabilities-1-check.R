# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Q2
mean(test)

#Q3
(1 - mean(test[disease == 1])) * mean(disease) / (1 - mean(test))

#Harvards answer:
mean(disease[test==0])


#Q4
mean(disease[test==1])

#Q5
mean(disease[test==1]) / mean(disease)
