##### 4 basic data structure in R #####
a <- 1:9
b <- list(first = 1:3, second = 4:6, third = 7:9)
c <- matrix(1:9, 3, 3)
d <- data.frame(first = 1:3, second = 4:6, third = 7:9) 


##### pracitce answer #####
### matrix ###
# 1.
A <- matrix(1:9, 3, 3, byrow = TRUE)

# 2.
B <- matrix(18:10, 3, 3, byrow = TRUE)

# 3.
C <- cbind(A, B)

# 4.
rowMeans(C)


### dataframe ###
# 1.
data <- CO2
tail(data)

# 2.
str(data)
summary(data)

# 3.
data[data$Type == 'Mississippi', ]
data$Type
data$Type == 'Mississippi'

# 4.
new_data = data[data$Type == 'Mississippi', ]
new_data[new_data$conc > 400, ]


### covariance & correlation coefficient ###
# 1.
mean(iris$Petal.Length)
median(iris$Petal.Length)

# 2.
var(iris$Petal.Width)
sd(iris$Petal.Width)

# 3.
library(EnvStats)
cv_length <- cv(iris$Petal.Length)
cv_width <- cv(iris$Petal.Width)
cv_length
cv_width

# 4.
cov(iris$Petal.Length, iris$Petal.Width)
cor(iris$Petal.Length, iris$Petal.Width)


### skewness & kurtosis ###
# 1.
numerator = sum((iris$Petal.Length - mean(iris$Petal.Length))^3)
denominator = (length(iris$Petal.Length-1))*(mean((iris$Petal.Length-mean(iris$Petal.Length))^2))^(3/2)
numerator / denominator

# 2.
moments::skewness(iris$Petal.Length)

# 3.
normal = rnorm(10000, 0, 1)
mean(normal)
sd(normal)

# 4.
numerator = sum((normal - mean(normal))^4)
denominator = (length(normal-1))*(mean((normal-mean(normal))^2))^(4/2)
numerator / denominator

moments::kurtosis(normal)
