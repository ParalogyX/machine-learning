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

#Q6
sum(ss_yv[1:3])/sum(ss_yv)
#in Harvards solution it is squared, but result is exactly the same
sum(s$d[1:3]^2) / sum(s$d^2)

#Q7
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))


#Q8
y_av <- rowMeans(y)
ud <- sweep(s$u, 2, s$d, FUN = "*")

qplot(y_av, ud[,1])

#harvards answer:
plot(s$u[,1]*s$d[1], rowMeans(y))


#Q9
my_image(s$v)


#Q10
plot(s$u[,1], ylim = c(-0.3, 0.3))
plot(t(s$v)[1,], ylim = c(-0.3, 0.3))

svd <- s$u[,1,drop=FALSE] * s$d[1] %*% t(s$v)[1,, drop = FALSE]
my_image(svd)
my_image(y)

#Harvards solution is interesting:
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)


#Q11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(y)

resid <- y - with(s,(u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q12
sum(s$d[1]^2)/sum(s$d^2) * 100
sum(s$d[2]^2)/sum(s$d^2) * 100
sum(s$d[3]^2)/sum(s$d^2) * 100
sum(s$d[1:2]^2)/sum(s$d^2) * 100
sum(s$d[1:3]^2)/sum(s$d^2) * 100

resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
my_image(resid)

#Q13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

my_image(y, zlim = c(min(y),max(y)))

with(s, my_image(sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]), zlim = c(min(y),max(y))))

resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(min(y),max(y)))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)


#Harvard's answer is better:
y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y))
my_image(y_hat, zlim = range(y))
my_image(y - y_hat, zlim = range(y))
