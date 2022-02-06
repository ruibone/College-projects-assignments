#############################################################
#                                                           #
#        Sample & Plot by 至煒                              #
#############################################################
rm(list=ls(all=T))

data = quakes 
head(data, 7)
data$depth
#The data set give the locations of 1000 seismic events of MB > 4.0.
#The events occurred in a cube near Fiji since 1964.

######################################################
##### sample() 從向量中隨機抽取元素

#取後不放回(不會拿到同一格)
sample(data$depth, 100, replace = FALSE)
#取後放回(允許拿到同一格)
sample(data$depth, 100, replace = TRUE)

##### 從dataframe隨機取得整筆資料
n     = nrow(data)
index = sample(1:n, 20, replace = FALSE)
data[index, ]
index = sort(index)
data[index, ]

######################################################
##### set.seed() 固定隨機抽取的方式i.e.不再隨機

#seed_66
set.seed(66)
sample(data$depth, 100, replace = FALSE)
sample(data$depth, 100, replace = TRUE)
n     = nrow(data)
index = sample(1:n, 20, replace = FALSE)
data[index, ]
index = sort(index)
data[index, ]

#seed_1013
set.seed(1013)
sample(data$depth, 100, replace = FALSE)
sample(data$depth, 100, replace = TRUE)
n     = nrow(data)
index = sample(1:n, 20, replace = FALSE)
data[index, ]
index = sort(index)
data[index, ]

######################################################
##### Hist 直方圖

hist(data$long)
hist(data$long, main='此直方圖標題', freq=FALSE)

######################################################
##### Pie  圓餅圖

time_spend = c('會計'       = 2, 
               '統計'       = 2.5, 
               '英文'       = 1, 
               '財管'       = 1.5, 
               '社交'       = 5, 
               '發呆'       = 2,
               '追劇打game' = 2,
               '睡覺'       = 8)
pie(time_spend)

pct  = round(time_spend / sum(time_spend) * 100)
lbls = paste(names(time_spend), pct, "%")
pie(time_spend, main = '此圓餅圖標題', lbls)

######################################################
##### Plot 分佈圖

plot(data$long, data$depth, main = '此分佈圖標題')
plot(data, main = '各變數分佈圖')

######################################################
##### Plot 函數圖

#常態分布pdf
f1 = function(x){
  mu       = 60
  variance = 400
  return((1/(2*pi*variance)^(1/2))*exp(-((x-mu)^2)/(2*variance)))
}

#(1/x)*sin(x)
f2 = function(x){
  return((1/x)*sin(x))
}

#log(x, 10) + 0.05*sin(x)
f3 = function(x){
  return(log(x, 10) + 0.05*sin(x))
}

#創造很密的x軸數值，並點出此x值所對應的函數值(type = 'l' : 以線連接相鄰之兩點)
x = seq(0, 100, 0.005)
plot(x, f1(x), type = 'l')
plot(x, f2(x), type = 'l')
plot(x, f3(x), type = 'l')

######################################################
##### Boxplot 箱型圖

boxplot(data$lat, main = '此箱型圖標題')

######################################################
##### 關閉右方之繪圖

dev.off()









