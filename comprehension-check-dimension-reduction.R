library(tidyverse)
library(dslabs)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Q1
ggplot() + geom_point(aes(tissue_gene_expression$x[, 1], tissue_gene_expression$x[, 2], color = tissue_gene_expression$y), size = 3)

#Harvard's answer creates dataframe - why? But it uses prcomp (Principal component analysis)
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q2
pred_av <- apply(tissue_gene_expression$x, 1, mean)
#or
#avgs <- rowMeans(tissue_gene_expression$x)

data.frame(pc_1 = pc$x[,1], pred_av = pred_av, tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pred_av, color = tissue)) + geom_point()

cor(pc$x[,1], pred_av)


#Q3

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


#Q4
data.frame(pc1 = pc$x[,1], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(tissue, pc1)) + geom_boxplot()



data.frame(pc7 = pc$x[,7], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(tissue, pc7)) + geom_boxplot()


#Harvrad propose to use cycle:
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#Q5
importance <- summary(pc)$importance

data.frame(pc = seq(1:length(importance[2,])), var_expl = importance[2,], var_expl_total = importance[3,]) %>%
  ggplot(aes(pc, var_expl)) + geom_point(color = "red", size = 2) + geom_point(aes(pc, var_expl_total), color = "blue", size = 2) + ylim(c(0, 1.1))

#easier and better by Harvard:
plot(summary(pc)$importance[3,])
