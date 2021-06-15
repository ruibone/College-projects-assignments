source("C:/Users/Darui Yen/OneDrive/桌面/高等作物育種學/CrossSimFunctions3.R")


#開花前選拔-------------------------------------------------------------------------------------

p.result <- matrix(0,1000,26)
g.result <- matrix(0,1000,26)
ge <- makege(a=rep(0.7, ng) , d=sample(c(1,-1), ng, replace=TRUE)*0.55, Ve=100) 

system.time(for (k in c(1:1000)){
  pop <- makebpop(size=2000, g=40, p=0.5)
  for(i in c(1:26)){
    pheno <- growpop(pop, ge, mu = 0)
    p.result[k,i] <- mean(pheno$p)
    g.result[k,i] <- mean(pheno$g)
    select.pheno <- order(pheno$p, decreasing = T)[1:200]
    select.pop <- selpop(pop, idx = select.pheno)
    pop <- ranmate(select.pop, size = 2000)
  }
}
)

before.p.result <- p.result
before.g.result <- g.result

before.mean <- rbind(colMeans(p.result),colMeans(g.result))
rownames(before.mean) <- c("p","g")
colnames(before.mean) <- c(0:12)
before.mean

#開花後選拔-------------------------------------------------------------------------------------

p.result <- matrix(0,1000,26)
g.result <- matrix(0,1000,26)
ge <- makege(a=rep(0.7, ng) , d=sample(c(1,-1), ng, replace=TRUE)*0.55, Ve=100) 

system.time(for (k in c(1:1000)){
  pop <- makebpop(size=2000, g=40, p=0.5)
  for(i in c(1:26)){
    pheno <- growpop(pop, ge, mu = 0)
    p.result[k,i] <- mean(pheno$p)
    g.result[k,i] <- mean(pheno$g)
    select.pheno <- order(pheno$p, decreasing = T)[1:200]
    select.pop <- selpop(pop, idx = select.pheno)
    Lf <- rgamete(select.pop, gender='f', line=200, plant=10)
    Lm <- rgamete(pop, gender='m', line=200, plant=10)
    pop <- c(Lf,Lm)
  }
})

after.p.result <- p.result
after.g.result <- g.result

after.mean <- rbind(colMeans(p.result),colMeans(g.result))
rownames(after.mean) <- c("p","g")
colnames(after.mean) <- c(0:12)
after.mean

#mean plot--------------------------------------------------------------------------------------

plot(c(0:12), before.g.result[1,], ylim = c(0,10), main = "Simulation Results for Mass Selection ", xlab = "generation", 
     ylab = "population mean genotypic value", col = 4)
for (i in c(1:1000)){
  lines(c(0:12), before.g.result[i,],col = 4)
  lines(c(0:12), after.g.result[i,],col = 2)
}
legend(x = 10.8, y = 1.8, pch = c(1,2), legend = c("before","after"), col = c(4,2), text.col = c(4,2))

plot(c(0:12), before.mean[2,], main = "Simulation Results for Mass Selection ", xlab = "generation", 
     ylab = "population mean genotypic value", ylim = c(0,10))
lines(c(0:12), before.mean[2,],col=4)
points(c(0:12), after.mean[2,],col=2)
lines(c(0:12), after.mean[2,], col = 2)
legend(x = 10.8, y = 1.8, pch = c(1,2), legend = c("before","after"), col = c(4,2), text.col = c(4,2))
