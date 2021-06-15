######matrix practice######

A <- matrix(1:9, 3, 3, byrow = T)
B <- matrix(18:10, 3, 3, byrow = T)
C <- cbind(A, B) #combine two matrices
rowMeans(A) 


######dataframe practice######

data("CO2")
print(CO2) #print all data
tail(CO2) #print last 6 rows
str(CO2)
filter1 <- CO2[CO2$Type == "Mississippi", ] 
filter2 <- subset(filter1, conc > 400) #select data which conc > 400 from filter1
