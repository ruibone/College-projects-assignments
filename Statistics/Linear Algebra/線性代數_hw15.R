fun <- function(x, y, z) {
  4*x^2+z*y^2
}
xs <- seq(-10, 10, by=0.1)
ys <- seq(-10, 10, by=0.1)

zmatrix7 <- zmatrix6 <- zmatrix5 <- zmatrix4 <- zmatrix3 <- zmatrix2 <- zmatrix1 <- matrix(NA, length(xs), length(ys))
for (i in 1:length(xs)) {
  for (j in 1:length(ys)) {
    zmatrix1[i,j] <- fun(xs[i], ys[j], 4)
    zmatrix2[i,j] <- fun(xs[i], ys[j], 2)
    zmatrix3[i,j] <- fun(xs[i], ys[j], 1)
    zmatrix4[i,j] <- fun(xs[i], ys[j], 0)
    zmatrix5[i,j] <- fun(xs[i], ys[j], -1)
    zmatrix6[i,j] <- fun(xs[i], ys[j], -2)
    zmatrix7[i,j] <- fun(xs[i], ys[j], -4)
  }
}

par(mfrow = c(2,4))
contour(x = xs, y = ys, z = zmatrix1, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= 4")))
contour(x = xs, y = ys, z = zmatrix2, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= 2")))
contour(x = xs, y = ys, z = zmatrix3, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= 1")))
contour(x = xs, y = ys, z = zmatrix4, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= 0")))
contour(x = xs, y = ys, z = zmatrix5, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= -1")))
contour(x = xs, y = ys, z = zmatrix6, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= -2")))
contour(x = xs, y = ys, z = zmatrix7, xlab = "x", ylab = "y", levels = c(0,1,2,4), xlim = c(-2,2), ylim = c(-2,2)
        , main = expression(paste(lambda, "= -4")))

################################################################################################################

yield <- data.frame(sales = c(126974,96933,86656,63438,55264,50976,39069,36156,35209,32416), 
                 profit = c(4224,3835,3510,3758,3939,1809,2946,359,2480,2413))
#aaaaa
cor(yield)
cov(yield)

#bbbbb
pca1 <- prcomp(yield)
pca1$rotation[,1]
pca2 <- prcomp(yield, scale = T)
pca2$rotation
