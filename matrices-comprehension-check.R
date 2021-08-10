library(matrixStats)

#Q1
x <- matrix(rnorm(100*10), 100, 10)

#Q2
dim(x)

nrow(x)

ncol(x)

#Q3
x <- x + seq(nrow(x))

x <- sweep(x, 1, 1:nrow(x),"+")

#Q4
x <- sweep(x, 2, 1:ncol(x), FUN = "+")

#Q5
rowMeans(x)
colMeans(x)

#Q6
if(!exists("mnist")) mnist <- read_mnist()

x <- mnist$train$images

x_vec <- as.vector(x)

mean(x_vec > 50 & x_vec < 205)

#Harvards answer includes tboxplot, but generally follows the same idea that me
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels
