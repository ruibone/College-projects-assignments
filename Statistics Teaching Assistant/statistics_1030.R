colnames(iris)

#coefficient of variation
sd(iris$Sepal.Length)/mean(iris$Sepal.Length)
sqrt(var(iris$Sepal.Length))/mean(iris$Sepal.Length)

#covariance
cov(iris$Sepal.Length,iris$Sepal.Width)

#correlation coefficient
cor(iris$Sepal.Length,iris$Sepal.Width)
cov(iris$Sepal.Length,iris$Sepal.Width)/(sd(iris$Sepal.Length)*sd(iris$Sepal.Width))

#coefficient of determination
cor(iris$Sepal.Length,iris$Sepal.Width)^2
summary(lm(data=iris, iris$Sepal.Length~iris$Sepal.Width))$r.squared

#coefficient of skewness
3*(mean(iris$Sepal.Length) - median(iris$Sepal.Length))/sd(iris$Sepal.Length)
mean((iris$Sepal.Length - mean(iris$Sepal.Length))^3) / (mean((iris$Sepal.Length - mean(iris$Sepal.Length))^2))^(3/2)

#normal distribution
rnorm(10, mean = 0, sd = 1)
shapiro.test(iris$Sepal.Length)
