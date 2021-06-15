setwd("C:/Users/Darui Yen/Desktop")
data = read.csv("perou.csv")

k = 1
deleterow = 1

for (i in c(1:9216)){
  if (sum(is.na(as.numeric(data[i,2:21]) + as.numeric(data[i,22:41]))) > 17){
    deleterow[k] <- i
    k = k+1
  }
}

filter.data <- data[-deleterow,]


outAll= matrix(NA, dim(filter.data)[1], 1) 
  
for (j in c(1:9204)){
  re = t.test(as.numeric(filter.data[j,2:21]) , as.numeric(filter.data[j,22:41]),paired = T)
  outAll[j,1] = re$p.value 
}

sum(outAll < 0.001)


