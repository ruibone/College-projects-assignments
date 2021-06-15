#隨機選---------------------------------------------------------------------------------------------------------

markernum <- c(50,100,200,400,800,1532)
matr1 <- matrix(NA, length(markernum), 100)
numcount = 1

for (num in markernum){
  for (rep in 1:100){
    marker <- sample(1:ncol(genoData), num, replace = F)
    geno <- genoData[,marker]
    mrkRel <- A.mat(geno, return.imputed= F)
    crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial",
                                          nRepeats=3)
    matr1[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
  }
  numcount <- numcount + 1
}

boxplot(t(matr1), xlab = "marker number", ylab = "accuracy", main = "Marker Number Comparison (random)", 
        names = c("50","100","200","400","800","all"))

#平均選----------------------------------------------------------------------------------------------------------

markernum <- c(50,100,200,400,800)
matr2 <- matrix(NA, length(markernum), 10)
numcount = 1

for (num in markernum){
  switch(numcount,
         marker <- seq(1, ncol(genoData), by = (ncol(genoData)/markernum[1]))[2:(markernum[1] + 1)],
         marker <- seq(1, ncol(genoData), by = (ncol(genoData)/markernum[2]))[2:(markernum[2] + 1)],
         marker <- seq(1, ncol(genoData), by = (ncol(genoData)/markernum[3]))[1:(markernum[3])],
         marker <- seq(1, ncol(genoData), by = (ncol(genoData)/markernum[4]))[1:(markernum[4])],
         marker <- seq(1, ncol(genoData), by = (ncol(genoData)/markernum[5]))[1:(markernum[5])])
  for (rep in 1:10){
    geno <- genoData[,marker]
    mrkRel <- A.mat(geno, return.imputed= F)
    crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=3)
    matr2[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
    marker <- marker + 1
    marker[marker > ncol(genoData)] <- marker[marker > ncol(genoData)] - ncol(genoData)
  }
  numcount <- numcount + 1
}

boxplot(t(matr2), xlab = "marker number", ylab = "accuracy", main = "Marker Number Comparison (interval)", 
        names = c("50","100","200","400","800"))

#篩marker後平均選-------------------------------------------------------------------------------------------------

editgeno <- genoData.raw
editgeno$index <- c(2:ncol(genoData),1)
repname <- editgeno$index[diff(genoData.raw$pos) == 0]
editgenoData <- t(editgeno[-repname,-c(1:3)])

markernum <- c(50,100,200,400,693)
matr3 <- matrix(NA, length(markernum), 10)
numcount = 1

for (num in markernum){
  switch(numcount,
         marker <- seq(1, ncol(editgenoData), by = (ncol(editgenoData)/markernum[1]))[2:(markernum[1] + 1)],
         marker <- seq(1, ncol(editgenoData), by = (ncol(editgenoData)/markernum[2]))[2:(markernum[2] + 1)],
         marker <- seq(1, ncol(editgenoData), by = (ncol(editgenoData)/markernum[3]))[1:(markernum[3])],
         marker <- seq(1, ncol(editgenoData), by = (ncol(editgenoData)/markernum[4]))[1:(markernum[4])],
         marker <- seq(1, ncol(editgenoData), by = (ncol(editgenoData)/markernum[5]))[1:(markernum[5])])
  for (rep in 1:10){
    geno <- editgenoData[,marker]
    mrkRel <- A.mat(geno, return.imputed= F)
    crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=10)
    matr3[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
    marker <- marker + 1
    marker[marker > ncol(editgenoData)] <- marker[marker > ncol(editgenoData)] - ncol(editgenoData)
  }
  numcount <- numcount + 1
}

boxplot(t(matr3), xlab = "marker number", ylab = "accuracy", main = "Marker Number Comparison (edit-interval)", 
        names = c("50","100","200","400","all"))

#k-means分群選----------------------------------------------------------------------------------------------------

markernum <- c(50,100,200,400,800)
matr4 <- matrix(NA, length(markernum), 1)
numcount <- 1

for (num in markernum){
  marker <- NA
  dist <- dist(t(genoData), method = "euclidean")
  kmean <- kmeans(dist, num)
  for (clu in 1:num){
    name <- (names(kmean$cluster[kmean$cluster == clu])[1])
    marker <- c(marker, name)
  }
  geno <- genoData[,colnames(genoData) %in% marker]
  mrkRel <- A.mat(geno, return.imputed= F)
  crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=10)
  matr4[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
  numcount <- numcount + 1
}

plot(1:5, matr4[,1], xlab = "marker number", ylab = "accuracy", main = "Marker Number Comparison (kmeans)", xaxt = "n")
axis(1, at = 1:5, labels = markernum)
lines(matr4, lty = 2)

#k-medoid分群選------------------------------------------------------------------------------------------------

markernum <- c(50,100,200,400,800)
matr5 <- matrix(NA, length(markernum), 1)
numcount <- 1

for (num in markernum){
  marker <- NA
  kmedoid <- pam(t(genoData), num)
  for (clu in 1:num){
    name <- (names(kmedoid$clustering[kmedoid$clustering == clu])[1])
    marker <- c(marker, name)
  }
  geno <- genoData[,colnames(genoData) %in% marker]
  mrkRel <- A.mat(geno, return.imputed= F)
  crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=10)
  matr5[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
  numcount <- numcount + 1
}

plot(1:5, matr5[,1], xlab = "marker number", ylab = "accuracy", main = "Marker Number Comparison (kmedoid)", xaxt = "n")
axis(1, at = 1:5, labels = markernum)
lines(matr5, lty = 2)

#GWAS順序選----------------------------------------------------------------------------------------------------

gwasgeno <- genoData.raw
gwasgeno[,1] <- rownames(gwasgeno)
colnames(gwasgeno)[1] <- "marker"
gwaspheno <- fhbData[,c(1,6)]

mrkRelMat <- A.mat(genoData, return.imputed= F)
gwas <- GWAS(pheno = gwaspheno, geno = gwasgeno, K = mrkRelMat)
order <- order(gwas$fhb.trans, decreasing = T)
gwas <- gwas[order,]

markernum <- c(50,100,200,400,800)
matr6 <- matrix(NA, length(markernum), 1)
numcount <- 1

for (num in markernum){
  marker <- gwas$marker[1:num]
  geno <- genoData[,colnames(genoData) %in% marker]
  mrkRel <- A.mat(geno, return.imputed= F)
  crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=10)
  matr6[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
  numcount <- numcount + 1
}

plot(1:5, matr6[,1], xlab = "marker number", ylab = "accuracy", main = "Marker Number Comparison (GWAS)", xaxt = "n")
axis(1, at = 1:5, labels = markernum)
lines(matr6, lty = 2)

#GAWS顯著才選------------------------------------------------------------------------------------------------

lod <- seq(0.5, 3, by = 0.5)
matr7 <- matrix(NA, length(lod), 1)
numcount <- 1

for (sig in lod){
  marker <- gwas$marker[gwas$fhb.trans > sig]
  geno <- genoData[,colnames(genoData) %in% marker]
  mrkRel <- A.mat(geno, return.imputed= F)
  crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=10)
  matr7[numcount,rep] <- mean(crossValOut$meanAccOverTrials)
  numcount <- numcount + 1
}

plot(1:6, matr7[,1], xlab = "minimum lod score", ylab = "accuracy", main = "Significant Marker " ,xaxt = "n")
axis(1, at = 1:6 , labels = lod)
lines(1:6, matr7[,1], lty = 2)

#GWAS顯著加隨機--------------------------------------------------------------------------------------------

markernum <- c(50,100,200,400,800)
lod <- c(1.5,2,2.5,3)
matr8 <- matrix(NA, length(markernum), length(lod))
vecrep <- rep(NA,10) 
numcount <- 1
sigcount <- 1

for (sig in lod){
  for (num in markernum){
    marker <- gwas$marker[gwas$fhb.trans > sig]
    siggeno <- genoData[,colnames(genoData) %in% marker]
    reducegenoData <- genoData[,!(colnames(genoData) %in% marker)]
    for (rep in 1:10){
      random <- sample(1:ncol(reducegenoData), num - length(marker), replace = F)
      rangeno <- reducegenoData[,random]
      geno <- cbind(siggeno, rangeno)
      mrkRel <- A.mat(geno, return.imputed= F)
      crossValOut <- runFoldCrossValidation(fhbData, geno="line", pheno="fhb.trans", K=mrkRel, nFolds=5, trial="trial", nRepeats=10)
      vecrep[rep] <- mean(crossValOut$meanAccOverTrials)
    }
    matr8[numcount,sigcount] <- mean(vecrep)
    numcount <- numcount + 1
  }
  sigcount <- sigcount + 1
  numcount <- 1
}

plot(1:4, matr8[1,], type = "l", lwd = 3, col = 2, xlab = "minimum lod score", ylab = "accuracy", xaxt = "n",
     main = "Mean for Different Marker Number (GWAS + random)", ylim = c(0.15,0.3))
axis(1, at = 1:4, labels = lod)
for (i in 1:nrow(matr8)){
  lines(1:4, matr8[i+1,], lwd = 3, col = i+2)
}
lines(1:4, matr7[3:6,1], lwd = 3, lty = 2)
legend("topright", legend = c("significant",markernum), col = 1:6, lwd = 3, cex = 0.8, lty = c(2,rep(1,5)))

#方法比較(#400)--------------------------------------------------------------------------------------------------

allmatr <- data.frame(random = matr1[4,], interval = matr2[4,], edit.interval = matr3[4,], 
                      kmeans = matr4[4,], kmedoid = matr5[4,], GWAS = matr6[4,], GWAS.random = mean(matr8[4,]))
boxplot(allmatr, main = "Marker Selecting Method Comparison", ylab = "accuracy", xlab = "selecting method", 
        names = colnames(allmatr))



