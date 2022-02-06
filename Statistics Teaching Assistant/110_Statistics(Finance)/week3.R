# page 1
# matrix 

# page 2
a <- 1:9
b <- matrix(a, 3, 3, byrow = T)
c <- matrix(0, 3, 3)

# page 3
d <- matrix(c(2, 3, 'Yes', TRUE), 2, 2)
class(d)
class(d[1,1])
class(d[2,2])

# page 4
e <- 1:9
f <- 10:18
g <- matrix(e, 3, 3)
h <- matrix(f, 3, 3)
i <- rbind(g, h)
j <- cbind(g, h)

# page 5
diag(3)
g+h
g-h
g*h
g/h
g%*%c(1:3)
g%*%h

# page 6
dim(g)
rowSums(g)
rowMeans(g)
colSums(g)
colMeans(g)

# page 7
# practice

# page 8
# dataframe

# page 9
data = data.frame('name' = c('A', 'B', 'C'), 'grade' = c(54, 81, 83), 'pass' = c(FALSE, TRUE, TRUE))
data$name

# page 10
dim(data)
ncol(data)
nrow(data)
str(data)
class(data)
class(data[,1])
class(data[,2])
class(data[,3])

# page 11
colnames(data)
rownames(data)
rownames(data) <- c('I', 'II', 'III')
dimnames(data)
dimnames(data) <- list(c('一', '二', '三'), c('名字', '成績', '通過'))

# page 12
data[,2]
data[1:2,]
data[-(1:2),]
data[data$通過 == 'FALSE', ]
data[data$通過 != 'FALSE', ]

# page 13
subset(data, subset = 通過 == 'TRUE', select = c('名字', '成績'))

# page 14
example_data <- read.csv('C:/Users/Darui Yen/OneDrive/桌面/nyc-flights.csv', header = T, sep = ',')
View(example_data)

# page 15
data()
cars

# page 16
str(iris)
head(iris)
tail(iris)

# page 17
# practice