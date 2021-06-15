clusterExport(cl, c("makef2", "growpop", "selpop", "ranmate", "rgamete", "sgamete", "ge", "psize", "ng", "nsel")) 
system.time(   
  bSel <- parSapply(cl, 1:5, function(x) {     
    C0 <- makef2(size=psize, g=ng)     
    mg <- vector("numeric", 12)     
    for (j in 1:12) {       
      p.C0 <- growpop(C0, ge)       
      mg[j] <- mean(p.C0$g)       
      sidx <- order(p.C0$p, decreasing = TRUE)[1:nsel]      
      C0s <- selpop(C0, idx = sidx)
      C1 <- c(sgamete(C0s, gender = "f", plant = psize/nsel),
              rgamete(C0s, gender = "m", line = psize, plant = 1))  
      C0 <- C1     
      }   
    p.C0 <- growpop(C0, ge)     
    mg <- c(mg, mean(p.C0$g)) 
    return(mg)  
  })
) 


#-----------------------------------------------------------------------------------

source("C:/Users/Darui Yen/OneDrive/桌面/高等作物育種學/CrossSimFunctions3.R")

#ge <- makege(a=c(2, 2, 2, 2, 2), d=c(1.6, -1.6, 1.6, 1.5, -1.5), Ve=100) 
#ng <- 5 
ng <- 40 
as <- rep(0.7, ng) 
ds <- sample(c(1,-1), ng, replace=TRUE)*0.55 
ge <- makege(a=as, d=ds, Ve=100) 
psize <- 2000 
nsel <- 200

cl <- makeCluster(detectCores())
clusterSetRNGStream(cl) 
clusterExport(cl, c("makebpop", "growpop", "selpop", "ranmate", "rgamete", "sgamete", "ge", "ng", "before"))

#開花前選拔-------------------------------------------------------------------------------------

system.time(
  before <- parSapply(cl, 1:1000, function(x) {
  pop <- makebpop(size=2000, g=ng, p=0.5)
  g.vector <- vector("numeric", 26)
    for(i in c(1:26)){
      pheno <- growpop(pop, ge, mu = 0)
      g.vector[i] <- mean(pheno$g)
      select.pheno <- order(pheno$p, decreasing = T)[1:200]
      select.pop <- selpop(pop, idx = select.pheno)
      pop <- ranmate(select.pop, size = 2000)
    }
  return(g.vector)
  })
)


#開花後選拔-------------------------------------------------------------------------------------

system.time(
  after <- parSapply(cl, 1:1000, function(x) {
    pop <- makebpop(size=2000, g=ng, p=0.5)
    g.vector <- vector("numeric", 26)
    for(i in c(1:26)){
      pheno <- growpop(pop, ge, mu = 0)
      g.vector[i] <- mean(pheno$g)
      select.pheno <- order(pheno$p, decreasing = T)[1:200]
      select.pop <- selpop(pop, idx = select.pheno)
      Lf <- rgamete(select.pop, gender='f', line=200, plant=10)
      Lm <- rgamete(pop, gender='m', line=200, plant=10)
      pop <- c(Lf,Lm)
    }
    return(g.vector)
  })
)


#mean plot--------------------------------------------------------------------------------------

plot(c(0:25), before[,1], ylim = c(0,30), main = "Simulation Results for Mass Selection ", xlab = "generation", 
     ylab = "population mean genotypic value", col = 4)
for (i in c(1:1000)){
  lines(c(0:25), before[,i], col = 4)
  lines(c(0:25), after[,i], col = 2)
}
legend(x = 22.4, y = 5.5, pch = c(1,2), legend = c("before","after"), col = c(4,2), text.col = c(4,2))


stopCluster(cl)
