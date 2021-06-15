library(qtl)

#資料讀取
----------------------------------------------
crox <- read.cross(format= "csvs", genfile = "geno.csv", phefile = "pheno.csv", 
                    na.strings=c("-","NA"), genotypes=c("a","b"), estimate.map=FALSE, 
                    map.function= "kosambi", crosstype = "riself")  
  
#個體篩選
----------------------------------------------
cg <- comparegeno(crox)
wh <- which(cg > 0.999, arr=TRUE) #找出分子標誌相似度過高之個體
wh <- wh[wh[,1] < wh[,2],]
crox <- subset(crox, ind=-wh[,2]) 

xo <- countXO(crox) #計算crossover過高的個體
hist(xo, breaks=50)
crox <- subset(crox, ind=(xo < 14 & xo > 1)) #刪掉重組次數較極端的個體

#分子標誌篩選
----------------------------------------------
crox <- calc.errorlod(crox, err=0.05) #計算可能genotype錯誤的LOD
top.errorlod(crox)
crox <- drop.markers(crox, c("X1_5876257","X1_20668556","X4_16337059"))

gt <- geno.table(cro)
gt[gt$P.value < 0.05/totmar(crox),]#計算分離不平衡情形
#可能為自然現象

dup <- findDupMarkers(crox, exact.only=F)#找出重複之分子標誌
#不在第一條染色體上，先保留


##建立遺傳圖譜
----------------------------------------------
crox <- est.rf(crox)
crox <- formLinkageGroups(crox, max.rf=0.3, min.lod=6, reorgMarkers= TRUE)

crox <- orderMarkers(crox, use.ripple = TRUE, window = 8, map.function = "kosambi")#windows = 8，耗時超過一小時


##局部翻轉第一條染色體之順序
----------------------------------------------
markernames(crox, chr = 1) #看名字可以知道位置順序
plotRF(crox)#或者看重組率圖判斷大概順序
crox <- switch.order(crox,1,c(11:1,12:48,88:49)) # 手動換(因為這條LG裡似乎又細分成3個LG)
# tryallpositons在此不適用，會有多個marker在同個位置有高LOD

crox <- est.map(crox)#遺傳圖譜建立完成
