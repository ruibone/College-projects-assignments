1 - pf(3.2, 3, 194) #pvalue
plot(df(seq(0,3.2,0.0001),3,194), xaxt = "n", main = "pdf") #pdf
axis(side=1, at=seq(0,32000,10000), labels = c("0","1","2","3"))
plot(pf(seq(0,3.2,0.0001),3,194), xaxt = "n", main = "cdf") #cdf
axis(side=1, at=seq(0,32000,10000), labels = c("0","1","2","3"))


pt(-2.08, 136)*2 #pvalue
(1 - pt(2.45, 136))*2 #pvalue
plot(dt(seq(-3,3,0.0001),136), xaxt = "n", main = "pdf") #pdf
axis(side=1, at=seq(0,60000,10000), labels = c("-3","-2","-1","0","1","2","3"))
plot(pt(seq(-3,3,0.0001),136), xaxt = "n", main = "cdf") #cdf
axis(side=1, at=seq(0,60000,10000), labels = c(-3:3))
