setwd("C:/Users/Darui Yen/OneDrive/桌面/線型模式/ALSM1_Final_2_2019/ALSM1_Final_2_2019")

y<-read.table(file = "dat.y.txt",sep="",header = T)
x<-read.table(file = "dat.x.txt",sep="",header = T)
dat<-cbind(y,x)
colnames(dat)[1]<-"y"

matr <- matrix(NA,10000,2)
matr[,1] <- seq(1:10000)
for (i in c(1:10000)){
  out <- lm(y~dat[,i+1],data = dat)
  matr[i,2] <- anova(out)[1,5]
} 

submatr <- subset(matr,matr[,2]<0.001)
submatr[,1]
dim(submatr)

outall <- lm(dat[,1]~dat[,14]+dat[,18]+dat[,20]+dat[,38]+dat[,505]+dat[,667]+dat[,3161]+dat[,3395]+dat[,4532]
             +dat[,4752]+dat[,5123]+dat[,6585]+dat[,6739]+dat[,7467]+dat[,7838]+dat[,8518]+dat[,8555]
             +dat[,9758]+dat[,9976],data = dat)
summary(outall)
---------------------------------------------------------------------------------------------------