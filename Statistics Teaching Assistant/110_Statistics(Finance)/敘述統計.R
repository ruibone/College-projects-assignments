#############################################################
#                                                           #
#        敘述統計 by 至煒                                   #
#############################################################
rm(list=ls(all=T))

####################
## 使用內建資料iris
data = iris
dim(data)
colnames(data)
data

####################
#### 中心趨勢

###平均
mean(data$Sepal.Length)
mean(data$Sepal.Width)
sum(data$Sepal.Length)/150
sum(data$Sepal.Width)/150

#刪除左右極端5%的資料
mean(data$Sepal.Length, trim = 0.05)
mean(data$Sepal.Width , trim = 0.05)

###眾數
Mode = function(x){
  ux = unique(x)
  return(
    ux[which.max(tabulate(match(x, ux)))]
  )
}
#
u = unique(data$Sepal.Length); u
m = match(data$Sepal.Length, u); m
t = tabulate(m); t
w = which.max(t); w
u[w]
#

Mode(data$Sepal.Length)
Mode(data$Sepal.Width)

###中位數
mL = median(data$Sepal.Length); mL
mW = median(data$Sepal.Width) ; mW
#n=150
sort(data$Sepal.Length)[75]
sort(data$Sepal.Length)[76]
(sort(data$Sepal.Length)[75] + sort(data$Sepal.Length)[76]) /2 == mL

####################
#### 資料分散程度

###最大值
max(data$Petal.Length)
max(data$Petal.Width)

###最小值
min(data$Petal.Length)
min(data$Petal.Width)

###全距
range(data$Petal.Length)
diff(range(data$Petal.Length))
diff(range(data$Petal.Width))

diff(range(data$Petal.Length)) ==
max(data$Petal.Length) - min(data$Petal.Length)

###百分位數k
k = 0.34
quantile(data$Petal.Length, k)

###四分位數
q1 = 0.25; Q1 = quantile(data$Petal.Length, q1); Q1
q2 = 0.5 ; Q2 = quantile(data$Petal.Length, q2); Q2
q3 = 0.75; Q3 = quantile(data$Petal.Length, q3); Q3

###四分位距:
IQR(data$Petal.Length)
IQR(data$Petal.Length) == Q3 - Q1

###Summary
summary(data)
by(data, data$Species, summary)

###變異數
n = 150

#樣本
var(data$Petal.Length)
sum((data$Petal.Length-mean(data$Petal.Length))^2)/(n-1)

#母體
sum((data$Petal.Length-mean(data$Petal.Length))^2)/(n)

###標準差
n = 150

#樣本
sd(data$Petal.Length)
var(data$Petal.Length)^(1/2)
(sum((data$Petal.Length-mean(data$Petal.Length))^2)/(n-1))^(1/2)

#母體
(sum((data$Petal.Length-mean(data$Petal.Length))^2)/(n))^(1/2)

####################
#### 機率密度函數
hist(data$Sepal.Length, breaks = 100)
hist(data$Sepal.Length, probability = 1, breaks = 100)
lines(density(data$Sepal.Length), col="red")

####################
#### 次數分配 
data$SL.size = ifelse(data$Sepal.Length < mean(data$Sepal.Length), "small", "big")

###Table

#絕對次數
table(data$Species)
table(data$SL.size)

#相對次戲
table(data$Species)/n
table(data$SL.size)/n

###Contingency Table

#絕對次數
table(data$Species, data$SL.size)
table(data$SL.size, data$Species)

#相對次戲
prop.table(table(data$Species, data$SL.size))
table(data$Species, data$SL.size)/n
prop.table(table(data$SL.size, data$Species))
table(data$SL.size, data$Species)/n

dev.off()





