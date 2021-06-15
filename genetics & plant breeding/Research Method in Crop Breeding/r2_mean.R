file2 <- read.delim(file = file.choose(),sep = "", header = T)

file2[,14] <- round(file2[,14],2)
matr2 <- matrix(0,101,2)
matr2[,2] <- seq(0,1,0.01)

for (i in c(1:100)){
  matr2[(i+1),1] <- mean(subset(file2,file2[,14] ==0.01*i)[,13],na.rm = T)
}


mean(subset(file2,file2[,14]>0.01 & file2[,14]<0.05)[,13]) - mean(subset(file2,file2[,14]>0.79 & file2[,14]<0.81)[,13])

ggplot(file, aes(Dist_bp, R.2)) + ggtitle("LD decay") + geom_smooth(se = F) + 
  scale_x_continuous(name="genetic distance (kb)", breaks=c(100,500,1000,2000,4000,6000,8000,10000), limits=c(0,10000)) + 
  ylab(expression(r^2)) 
