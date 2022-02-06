##### covariance & correlation coefficient #####
library(EnvStats)

# page 2
sum((iris$Sepal.Length - mean(iris$Sepal.Length))^2) / (length(iris$Sepal.Length) - 1)
var(iris$Sepal.Length)
sqrt(sum((iris$Sepal.Length - mean(iris$Sepal.Length))^2) / (length(iris$Sepal.Length) - 1))
sd(iris$Sepal.Length)

# page 3
# coefficient of variation

# page 4
cv1 <- sd(iris$Sepal.Length) / mean(iris$Sepal.Length)
cv2 <- sqrt(var(iris$Sepal.Length)) / mean(iris$Sepal.Length)
cv3 <- cv(iris$Sepal.Length)
cv1 == cv2
cv1 == cv3

# page 5
sum((iris$Sepal.Length - mean(iris$Sepal.Length))*(iris$Sepal.Width - mean(iris$Sepal.Width))) / (length(iris$Sepal.Length)-1)
cov(iris$Sepal.Length, iris$Sepal.Width)

# page 6-7
# correlation coefficient 

# page 8
# correlation of determination

# page 9
cov(iris$Sepal.Length, iris$Sepal.Width) / (sd(iris$Sepal.Length)*sd(iris$Sepal.Width))
cor(iris$Sepal.Length, iris$Sepal.Width)

cor(iris$Sepal.Length, iris$Sepal.Width)^2
summary(lm(data = iris, iris$Sepal.Length~iris$Sepal.Width))$r.squared

##### skewness & kurtosis #####
install.packages('moments')
library(moments)

# page 2
# skewness

# page 3
numerator = sum((iris$Sepal.Length - mean(iris$Sepal.Length))^3)
denominator = (length(iris$Sepal.Length-1))*(mean((iris$Sepal.Length-mean(iris$Sepal.Length))^2))^(3/2)
skew1 = numerator / denominator

skew2 = skewness(iris$Sepal.Length)

# page 4
hist(iris$Sepal.Length, freq = F, main = 'Skewness', xlab = 'Sepal Length')
lines(density(iris$Sepal.Length), lwd = 3)
abline(v = median(iris$Sepal.Length), col = 'blue', lty = 3, lwd = 5)
abline(v = mean(iris$Sepal.Length), col = 'red', lty = 3, lwd = 5)

# page 5
3*(mean(iris$Sepal.Length) - median(iris$Sepal.Length)) / sqrt(mean((iris$Sepal.Length - mean(iris$Sepal.Length))^2))

# page 6
numerator = sum((iris$Sepal.Length - mean(iris$Sepal.Length))^4)
denominator = (length(iris$Sepal.Length)-1)*(mean((iris$Sepal.Length-mean(iris$Sepal.Length))^2))^(4/2)
kurtosis1 = numerator / denominator

kurtosis2 = kurtosis(iris$Sepal.Length)

# page 7
data = rnorm(10, mean = 0, sd = 1)
mean(data)
sd(data)