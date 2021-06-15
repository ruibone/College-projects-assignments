for_fac <- 1
for (i in 1:10){
  for_fac <- for_fac*i
}

while_fac <- 1
j <- 1
while (TRUE){
  while_fac <- while_fac*j
  j <- j+1
  if (j>10) break 
}

c(for_fac, while_fac)
####

x <- c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000)
index <- 1
while (TRUE){
  if(is.na(x[index])){
    cat("The index of NA value in vector x is", index, ".", "\n" )
    break
  }
  index <- index+1
}
###

BMI <- fread("C:/Users/Darui Yen/OneDrive/орн▒/BMIrepeated.csv", header = T)
par(mfrow = c(1,2))
plot(seq(0,9,3), BMI[1,c(4:7)], type = "b", main = "Drug Group", ylab = "BMI", xlab = "month", ylim = c(18,28))
for (i in 2:10) lines(seq(0,9,3), BMI[i,c(4:7)], type = "b", col = i)
legend("topright",bty="n", c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10"),lty=1,col=(1:10))

plot(seq(0,9,3), BMI[51,c(4:7)], type = "b", main = "Drug Group", ylab = "BMI", xlab = "month", ylim = c(18,28))
for (i in 2:10) lines(seq(0,9,3), BMI[(i+50),c(4:7)], type = "b", col = i)
legend("topright",bty="n", c("ID51","ID52","ID53","ID54","ID55","ID56","ID57","ID58","ID59","ID60"),lty=1,col=(1:10))
###

subject <- c(10, 12, 4, 16, 8)
mean <- subject*100/sum(subject)
lbls <- c("US", "UK", "Australia", "Germany", "France")
for (i in 1:length(lbls)){
  lbls[i] <- paste(lbls[i], as.character(mean[i]), "%")
}
pie3D(subject, labels=lab, explode=0.1, main="Pie Chart of Countries ")
