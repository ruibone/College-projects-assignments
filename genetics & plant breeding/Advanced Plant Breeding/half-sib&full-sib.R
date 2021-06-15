source("C:/Users/Darui Yen/OneDrive/桌面/高等作物育種學/CrossSimFunctions3.R")


as <- rep(0.7, 40)
ds <- sample(c(1,-1), 40, replace=TRUE)*0.55 
ve <- 100
ge <- makege(a = as, d = ds, Ve = ve) 

#variance-------------------------------------------------------------------------------------------------
p <- mean(sapply(1:1000, function(x) {
  pop <- makebpop(size = 2000, g = 40, p = 0.5)
  p <-  0.5 + (colMeans(pop$male+pop$female))*0.5
}))
q <- 1-p
alpha <- as + (q-p)*ds
va <- sum(2*p*q*alpha*alpha)
vd <- sum((2*p*q*ds)**2)
vg <- va + vd
vp <- vg + ve
i <- dnorm(qnorm(1-0.1))/0.1
R.1 <- (i*(1/4)*sqrt(va))/sqrt((1/4)*va+ve/10)*sqrt(va)
R.2 <- (i*(1/2)*sqrt(va))/sqrt((1/4)*va+ve/10)*sqrt(va)
R.3 <- (i*(1/2)*sqrt(va))/sqrt((1/2)*va+(1/4)*vd+ve/10)*sqrt(va)
R.4 <- (i*sqrt(va))/sqrt(va+(1/4)*vd+ve/10)*sqrt(va)
  
  
cl <- makeCluster(detectCores())
clusterSetRNGStream(cl) 
clusterExport(cl, c("makebpop", "growpop", "selpop", "ranmate", "rgamete", "sgamete", "ge", "select.self"))

#半同胞家系留存種子法-------------------------------------------------------------------------------------


half.reserve <- parSapply(cl, 1:1000, function(x) {
  pop <- makebpop(size = 2000, g = 40, p = 0.5)
  p.vector <- vector("numeric", 11)
  select.index <- sample(1:2000, 200, replace = F)
  select.pop <- selpop(pop, idx = select.index)
  Lf1 <- sgamete(select.pop, gender='f', plant=10) 
  Lm1 <- rgamete(select.pop, gender='m', line=2000, plant=1, RPL = T)
  pop1 <- c(Lf1, Lm1)
  eva.Lf <- sgamete(select.pop, gender='f', plant=10) 
  eva.Lm <- rgamete(select.pop, gender='m', line=2000, plant=1, RPL = T)
  eva.pop <- c(eva.Lf, eva.Lm)
  pheno <- growpop(eva.pop, ge, mu = 0)
  p.vector[1] <- mean(pheno$p)
  family <- split(pheno$p, rep(1:200, each = 10))
  family.pheno <- sapply(family, mean)
  order.pheno <- order(family.pheno, decreasing = T)[1:20]
  select.pheno <- lapply(order.pheno, function(x) ((x*10):(x*10-9)))
  select.pop <- selpop(pop1, idx = unlist(select.pheno))
  for(i in c(1:10)){
    Lf <- sgamete(select.pop, gender='f', plant=10) 
    Lm <- rgamete(select.pop, gender='m', line=2000, plant=1, RPL = T)
    pop <- c(Lf, Lm)
    eva.Lf <- sgamete(select.pop, gender='f', plant=10) 
    eva.Lm <- rgamete(select.pop, gender='m', line=2000, plant=1, RPL = T)
    eva.pop <- c(eva.Lf, eva.Lm)
    pheno <- growpop(eva.pop, ge, mu = 0)
    p.vector[i+1] <- mean(pheno$g)
    family <- split(pheno$p, rep(1:200, each = 10))
    family.pheno <- sapply(family, mean)
    order.pheno <- order(family.pheno, decreasing = T)[1:20]
    select.pheno <- lapply(order.pheno, function(x) ((x*10):(x*10-9)))
    select.pop <- selpop(pop, idx = unlist(select.pheno))
  }
  return(p.vector)
})

mean1 <- t(data.frame(colMeans(t(half.reserve))))
colnames(mean1) <- c(0:10)
rownames(mean1) <- "genotypic value"
gain1 <- t(data.frame(diff(as.vector(mean1))))
colnames(gain1) <- c(1:10)
rownames(gain1) <- "genetic gain"

plot(c(0:10), t(half.reserve[,1]), ylim = c(0,25), main = "half-sib / ramnant seed", xlab = "generation", 
     ylab = "population mean genotypic value", col = 4)
for (i in c(1:1000)){
  lines(c(0:10), t(half.reserve[,i]), col = 4)
}
lines(c(0:10), colMeans(t(half.reserve)), col = "black", lwd = 4)
legend("bottomright" , legend = "mean", col="black", lty = 1, lwd = 4, box.lty = 0)


#半同胞家系親本自交後裔-------------------------------------------------------------------------------------

half.self <- parSapply(cl, 1:1000, function(x) {
  pop <- makebpop(size = 2000, g = 40, p = 0.5)
  p.vector <- vector("numeric", 11)
  select.index <- sample(1:2000, 200, replace = F)
  select.pop <- selpop(pop, idx = select.index)
  p.vector[1] <- mean(growpop(select.pop, ge, mu = 0)$p)
  self.Lf <- sgamete(select.pop, gender = "f", plant = 10)
  self.Lm <- sgamete(select.pop, gender = "m", plant = 10)
  select.self <- c(self.Lf, self.Lm)
  eva.Lf <- sgamete(select.pop, gender='f', plant=10) 
  eva.Lm <- rgamete(pop, gender='m', line=2000, plant=1)
  eva.pop <- c(eva.Lf, eva.Lm)
  pheno <- growpop(eva.pop, ge, mu = 0)
  p.vector[1] <- mean(pheno$p)
  family <- split(pheno$p, rep(1:200, each = 10))
  family.pheno <- sapply(family, mean)
  order.pheno <- order(family.pheno, decreasing = T)[1:20]
  select.pheno <- unlist(lapply(order.pheno, function(x) ((x*10):(x*10-9))))
  select.pop <- selpop(select.self, idx = select.pheno)
  for(i in c(1:10)){
    Lf <- sgamete(select.pop, gender='f', plant=10) 
    Lm <- rgamete(select.pop, gender='m', line=2000, plant=1)
    pop <- c(Lf, Lm)
    select.pop <- selpop(pop, size = 200)
    self.Lf <- sgamete(select.pop, gender = "f", plant = 10)
    self.Lm <- sgamete(select.pop, gender = "m", plant = 10)
    select.self <- c(self.Lf, self.Lm)
    eva.Lf <- sgamete(select.pop, gender='f', plant=10) 
    eva.Lm <- rgamete(pop, gender='m', line=2000, plant=1)
    eva.pop <- c(eva.Lf, eva.Lm)
    pheno <- growpop(eva.pop, ge, mu = 0)
    p.vector[i+1] <- mean(pheno$p)
    family <- split(pheno$p, rep(1:200, each = 10))
    family.pheno <- sapply(family, mean)
    order.pheno <- order(family.pheno, decreasing = T)[1:20]
    select.pheno <- unlist(lapply(order.pheno, function(x) ((x*10):(x*10-9))))
    select.pop <- selpop(select.self, idx = select.pheno)
   
  }
  return(p.vector)
})

mean2 <- t(data.frame(colMeans(t(half.self))))
colnames(mean2) <- c(0:10)
rownames(mean2) <- "genotypic value"
gain2 <- t(data.frame(diff(as.vector(mean2))))
colnames(gain2) <- c(1:10)
rownames(gain2) <- "genetic gain"

plot(c(0:10), t(half.self[,1]), ylim = c(0,25), main = "half-sib / self", xlab = "generation", 
     ylab = "population mean genotypic value", col = 3)
for (i in c(1:1000)){
  lines(c(0:10), t(half.self[,i]), col = 3)
}
lines(c(0:10), colMeans(t(half.self)), col = "black", lwd = 4)
legend("bottomright" , legend = "mean", col="black", lty = 1, lwd = 4, box.lty = 0)


#全同胞家系留存種子法-----------------------------------------------------------------------------------------

full.sib <- parSapply(cl, 1:1000, function(x) {
  pop <- makebpop(size = 2000, g = 40, p = 0.5)
  p.vector <- vector("numeric", 11)
  select.index <- sample(1:2000, 400, replace = F)
  select.f <- selpop(pop, idx = select.index[1:200])
  select.m <- selpop(pop, idx = select.index[201:400])
  full.Lf <- sgamete(select.f, gender = "f", plant = 10)
  full.Lm <- sgamete(select.m, gender = "m", plant = 10)
  full.pop <- c(full.Lf, full.Lm)
  eva.Lf <- sgamete(select.f, gender = "f", plant = 10)
  eva.Lm <- sgamete(select.m, gender = "m", plant = 10)
  eva.pop <- c(eva.Lf, eva.Lm)
  pheno <- growpop(eva.pop, ge, mu = 0)
  p.vector[1] <- mean(pheno$p)
  family <- split(pheno$p, rep(1:200, each = 10))
  family.pheno <- sapply(family, mean)
  order.pheno <- order(family.pheno, decreasing = T)[1:20]
  select.pheno <- unlist(lapply(order.pheno, function(x) ((x*10):(x*10-9))))
  select.pop <- selpop(full.pop, idx = select.pheno)
  for(i in c(1:10)){
    f.pop <- sgamete(select.pop, gender = "f", plant = 10)
    m.pop <- rgamete(select.pop, gender = "m", line = 2000, plant = 1, RPL = TRUE ) 
    pop <- c(f.pop, m.pop) 
    p.vector[1+i] <- mean(growpop(pop,ge,mu=0)$g)
    select.f <- selpop(pop, idx = select.index[1:200])
    select.m <- selpop(pop, idx = select.index[201:400])
    full.Lf <- sgamete(select.f, gender = "f", plant = 10)
    full.Lm <- sgamete(select.m, gender = "m", plant = 10)
    full.pop <- c(full.Lf, full.Lm)
    eva.Lf <- sgamete(select.f, gender = "f", plant = 10)
    eva.Lm <- sgamete(select.m, gender = "m", plant = 10)
    eva.pop <- c(eva.Lf, eva.Lm)
    pheno <- growpop(eva.pop, ge, mu = 0)
    family <- split(pheno$p, rep(1:200, each = 10))
    family.pheno <- sapply(family, mean)
    order.pheno <- order(family.pheno, decreasing = T)[1:20]
    select.pheno <- unlist(lapply(order.pheno, function(x) ((x*10):(x*10-9))))
    select.pop <- selpop(full.pop, idx = select.pheno)
  }
  return(p.vector)
})

mean3 <- t(data.frame(colMeans(t(full.sib))))
colnames(mean3) <- c(0:10)
rownames(mean3) <- "genotypic value"
gain3 <- t(data.frame(diff(as.vector(mean3))))
colnames(gain3) <- c(1:10)
rownames(gain3) <- "genetic gain"

plot(c(0:10), t(full.sib[,1]), ylim = c(0,25), main = "full-sib", xlab = "generation", 
     ylab = "population mean genotypic value", col = 2)
for (i in c(1:1000)){
  lines(c(0:10), t(full.sib[,i]), col = 2)
}
lines(c(0:10), colMeans(t(full.sib)), col = "black", lwd = 4)
legend("bottomright" , legend = "mean", col="black", lty = 1, lwd = 4, box.lty = 0)


#S1自交後裔法-----------------------------------------------------------------------------------------

s1.self <- parSapply(cl, 1:1000, function(x) {
  pop <- makebpop(size = 2000, g = 40, p = 0.5)
  p.vector <- vector("numeric", 11)
  select.index <- sample(1:2000, 200, replace = F)
  select.pop <- selpop(pop, idx = select.index)
  self.Lf <- sgamete(select.pop, gender = "f", plant = 10)
  self.Lm <- sgamete(select.pop, gender = "m", plant = 10)
  select.self <- c(self.Lf, self.Lm)
  eva.Lf <- sgamete(select.pop, gender = 'f', plant = 10) 
  eva.Lm <- sgamete(select.pop, gender = "m", plant = 10)
  eva.pop <- c(eva.Lf, eva.Lm)
  pheno <- growpop(eva.pop, ge, mu = 0)
  p.vector[1] <- mean(pheno$p)
  family <- split(pheno$p, rep(1:200, each = 10))
  family.pheno <- sapply(family, mean)
  order.pheno <- order(family.pheno, decreasing = T)[1:20]
  select.pheno <- unlist(lapply(order.pheno, function(x) ((x*10):(x*10-9))))
  select.pop <- selpop(select.self, idx = select.pheno)
  for(i in c(1:10)){
    Lf <- sgamete(select.pop, gender='f', plant=10) 
    Lm <- rgamete(select.pop, gender='m', line=2000, plant=1)
    pop <- c(Lf, Lm)
    select.pop <- selpop(pop, size = 200)
    self.Lf <- sgamete(select.pop, gender = "f", plant = 10)
    self.Lm <- sgamete(select.pop, gender = "m", plant = 10)
    select.self <- c(self.Lf, self.Lm)
    eva.Lf <- sgamete(select.pop, gender = 'f', plant = 10) 
    eva.Lm <- sgamete(select.pop, gender = "m", plant = 10)
    eva.pop <- c(eva.Lf, eva.Lm)
    pheno <- growpop(eva.pop, ge, mu = 0)
    p.vector[i+1] <- mean(pheno$p)
    family <- split(pheno$p, rep(1:200, each = 10))
    family.pheno <- sapply(family, mean)
    order.pheno <- order(family.pheno, decreasing = T)[1:20]
    select.pheno <- unlist(lapply(order.pheno, function(x) ((x*10):(x*10-9))))
    select.pop <- selpop(select.self, idx = select.pheno)
  }
  return(p.vector)
})

mean4 <- t(data.frame(colMeans(t(s1.self))))
colnames(mean4) <- c(0:10)
rownames(mean4) <- "genotypic value"
gain4 <- t(data.frame(diff(as.vector(mean4))))
colnames(gain4) <- c(1:10)
rownames(gain4) <- "genetic gain"

plot(c(0:10), t(s1.self[,1]), ylim = c(0,25), main = "S1 - self", xlab = "generation", 
     ylab = "population mean genotypic value", col = 8)
for (i in c(1:1000)){
  lines(c(0:10), t(s1.self[,i]), col = 8)
}
lines(c(0:10), colMeans(t(s1.self)), col = "black", lwd = 4)
legend("bottomright" , legend = "mean", col="black", lty = 1, lwd = 4, box.lty = 0)


#statistics&plot-------------------------------------------------------------------------------------------------
genetic.mean <- t(data.frame(colMeans(t(half.reserve)), colMeans(t(half.self)), 
                             colMeans(t(full.sib)), colMeans(t(s1.self))))
rownames(genetic.mean) <- c("method1", "method2", "method3", "method4")
colnames(genetic.mean) <- c(0:10)
genetic.gain <- t(data.frame(diff(as.vector(mean1)), diff(as.vector(mean2)), 
                             diff(as.vector(mean3)), diff(as.vector(mean4))))
rownames(genetic.gain) <- c("method1", "method2", "method3", "method4")
colnames(genetic.gain) <- c(1:10)

first_generation <- t(data.frame(c(R.1, R.2, R.3, R.4), genetic.gain[,1]))
rownames(first_generation) <- c("theoretical", "simulated")
colnames(first_generation) <- c("method1", "method2", "method3", "method4")

genetic.mean
genetic.gain
first_generation


plot(0:10, rep(-10,11), ylim = c(0, 25), main = "method comparison", xlab = "generation", ylab = "genotypic mean")
lines(0:10, mean1, col = 4, lwd = 5)
lines(0:10, mean2, col = 3, lwd = 5)
lines(0:10, mean3, col = 2, lwd = 5)
lines(0:10, mean4, col = 8, lwd = 5)
legend("bottomright", legend = c("method1", "method2", "method3", "method4"), col = c(4,3,2,8), lwd = 3, box.lty = 0)

plot(0:10, rep(-10,11), ylim = c(0, 25), main = "method comparison", xlab = "generation", ylab = "genotypic value")
for (i in c(1:50)){
  lines(0:10, half.reserve[,i], col = "4", lwd = 0.1)
  lines(0:10, half.self[,i], col = 3, lwd = 0.1)
  lines(0:10, full.sib[,i], col = 2, lwd = 0.1)
  lines(0:10, s1.self[,i], col = 8, lwd = 0.1)
}
legend("bottomright", legend = c("method1", "method2", "method3", "method4"), col = c(4,3,2,8), lwd = 3, box.lty = 0)
#---------------------------------------------------------------------------------------------------------------
stopCluster(cl)


