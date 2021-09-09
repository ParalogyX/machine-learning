library(tidyverse)

set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))


#Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q3
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y, 2, FUN = function(x) sum(x^2))

ss_yv <- apply((y %*% s$v), 2, FUN = function(x) sum(x^2))
  
round(sum(ss_y)) == round(sum (ss_yv))
  
print(sum(ss_y))

#a bit easier by Harvards:
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

#Q4
qplot(ss_y, 1:length(ss_y))
qplot(ss_yv, 1:length(ss_yv))

#or?
qplot(1:length(ss_y), ss_y)
qplot(1:length(ss_yv), ss_yv)

#Second option is correct, but I should know about function:
plot(ss_y) 
plot(ss_yv)

#Q5
qplot(s$d, sqrt(ss_yv))

#Q3
sum(ss_yv[1:3])/sum(ss_yv)
#in Harvards solution it is squared, but result is exactly the same
sum(s$d[1:3]^2) / sum(s$d^2)
