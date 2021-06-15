setwd("C:/Users/Darui Yen/OneDrive/桌面/育種試驗資料分析")
Raw <- read.csv("Marker alleles useful.csv")

markers <- unique(Raw$Marker)
Raw$Allele2[is.na(Raw$Allele2)] <- Raw$Allele1[is.na(Raw$Allele2)]

R1 <- Raw[,c(1,2,3)]
colnames(R1)[3] <- "Allele"
R2 <- Raw[,c(1,2,4)]
colnames(R2)[3] <- "Allele"
Raw1 <- rbind(R1,R2)

Marker <- c()
Allele <- c()
Afreq<- c()
for(i in c(1:length(markers))){
  x<- Raw1[Raw1$Marker==markers[i],]
  countx <- with(x,table(Allele,Sample))
  sumx<- colSums(countx)	
  freqx <- t(t(countx)/sumx)
  Marker <- rbind(Marker,matrix(markers[i],nrow(freqx),1))	
  Afreq <- rbind(Afreq,freqx)
}	

Allele <- rownames(Afreq)
rownames(Afreq) <- NULL	
Result <- data.frame(Marker, Allele, Afreq) 

OTU <- names(Result)[-c(1,2)]
X <- t(as.matrix(Result[,-c(1,2)]))
D <- dist(X, method="euclidean")/sqrt(2*length(Markers))	


mds <- cmdscale(D, eig=TRUE)
eig <- mds$eig/sum(mds$eig[mds$eig>0])*100
sym <- c(rep(16,5),rep(1,9),7,1)

par(mar=c(5,4,1,2))	
plot(mds$points[,1],mds$points[,2], asp=1, pch=sym,
     xlab=paste("Principal coordinate 1 (",format(eig[1], digits=3), "%)", sep=""),	
     ylab=paste("Principal coordinate 2 (",format(eig[2], digits=3), "%)", sep=""))	
text(mds$points[,1],mds$points[,2], OTU, pos=3, cex=0.67)	

plot(c(1:length(eig)),eig, xlab="Principal Coordinate", ylab="Explained variance [%]",type="b")	
