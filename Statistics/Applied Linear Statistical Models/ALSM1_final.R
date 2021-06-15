setwd("C:/Users/Darui Yen/OneDrive/桌面/線型模式/ALSM1_Final_2_2019/ALSM1_Final_2_2019")

y<-read.table(file = "dat.y.txt",sep="",header = T)
x<-read.table(file = "dat.x.txt",sep="",header = T)
dat<-cbind(y,x)
colnames(dat)[1]<-"y"

matr <- matrix(NA,10000,2)
matr[,1] <- seq(1:10000)
for (i in c(1:10000)){
  out <- lm(y~dat[,i+1],data = dat)
  matr[i,2] <- summary(out)$coefficients[2,4]
} 

submatr <- subset(matr,matr[,2]<0.0002)
submatr[,1]
dim(submatr)

outall <- lm(dat[,1]~dat[,14]+dat[,18]+dat[,38]+dat[,667]+dat[,3395]+dat[,4532]
             +dat[,4752]+dat[,9758],data = dat)
summary(outall)
#---------------------------------------------------------------------------------------------------
matrinter <- matrix(NA,28,3)
r = 1
for (i in c(14,18,38,667,3395,4532,4572,9758)){
  for (j in c(14,18,38,667,3395,4532,4572,9758)){
    if (i<j){
      outinter <- lm(dat[,1]~dat[,i]*dat[,j])
      matrinter[r,1] <- i
      matrinter[r,2] <- j
      matrinter[r,3] <- summary(outinter)$coefficients[4,4]
      r = r+1
    }
  }
}
matrinter
submatrinter <- subset(matrinter,matrinter[,3] < 0.0001)
submatrinter

      
matrinter3 <- matrix(NA,56,4)
r = 1
for (i in c(14,18,38,667,3395,4532)){
  for (j in c(18,38,667,3395,4532,4572)){
    for (k in c(38,667,3395,4532,4572,9758)){  
      if (i<j&j<k){
        outinter3 <- lm(dat[,1]~dat[,i]*dat[,j]*dat[,k],data=dat)
        matrinter3[r,1] <- i
        matrinter3[r,2] <- j
        matrinter3[r,3] <- k
        matrinter3[r,4] <- summary(outinter3)$coefficients[8,4]
        r = r+1
      }
    }  
  }
}
matrinter3
submatrinter3 <- subset(matrinter3,matrinter3[,3] < 0.05)
submatrinter3
#----------------------------------------------------------------------------------------

fit <- lm(y~1,data=dat)
all <- lm(dat[,1]~dat[,14]+dat[,18]+dat[,38]+dat[,667]+dat[,3395]+dat[,4532]+dat[,4752]+dat[,9758],data=dat)
allinter <- lm(dat[,1]~dat[,14]+dat[,18]+dat[,38]+dat[,667]+dat[,3395]+dat[,4532]
                    +dat[,4752]+dat[,9758]+dat[,14]*dat[,18]+dat[,14]*dat[,38]+
               dat[,3395]*dat[,18]+dat[,38]*dat[,667],data = dat)
forward.lm <- step(fit,direction="forward",scope=list(upper=all))
step.lm <- step(fit,scope=list(upper=all),direction="both")
step.lm
stepinter.lm <- step(fit,scope=list(upper=allinter),direction="both")
stepinter.lm

final <- lm(y ~ dat[, 14] + dat[, 38] + dat[, 18] + dat[, 667] + dat[, 3395] + dat[, 4532] + dat[, 9758], data = dat)
summary(final)
finalinter <- lm(y ~ dat[, 14] + dat[, 38] + dat[, 18] + dat[, 667] + 
  dat[, 4532] + dat[, 9758] + dat[, 14]:dat[, 38] + dat[, 14]:dat[, 18] + dat[, 38]:dat[, 667], data = dat)
summary(finalinter)

end <- lm(y ~ dat[, 14] + dat[, 38] + dat[, 18] + dat[, 667] + 
            dat[, 4532] + dat[, 14]:dat[, 38] + dat[, 14]:dat[, 18] + dat[, 38]:dat[, 667], data = dat)
summary(end)

bi=extractAIC(finalinter,k=log(length(dat[,1])))[2]
biend=extractAIC(end,k=log(length(dat[,1])))[2]
