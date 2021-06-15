library(VariantAnnotation)

sample.sheet= read.csv(file = "C:/Users/Darui Yen/OneDrive/桌面/育種試驗資料分析/Rice_SampleSheet.csv")
vcf.sample.name= sprintf("S%03d", subset(sample.sheet, country== "TW")$"SID")

scan.param= ScanVcfParam(
  fixed= c("REF", "ALT", "QUAL"), info= c("QD"),
  geno= c("GT", "GQ"), samples = vcf.sample.name)

##### inport VCF with tabix index needed when using "which" argument
##### (1) compress and index file once!
# compressVcf <- bgzip(vcf.file)
# idx <- indexTabix(compressVcf, "vcf")
# tab <- TabixFile(compressVcf, idx)

##### (2) when VCF file compressed and indexed
compressVcf= "C:/Users/Darui Yen/OneDrive/桌面/育種試驗資料分析/03-08_Sample_96_Genotype.vcf.bgz"
tab <- TabixFile(compressVcf)

##### readVcf
vcf1= readVcf(file = tab, param = scan.param)
vcf1
##################################################
# filter markers
QUAL= fixed(vcf1)$"QUAL"
QD= info(vcf1)$"QD"; QD[is.na(QD)]= 0
GT= geno(vcf1)$"GT"

Geno.tab= apply(GT, 1, function(x) table(factor(x, levels= c("0/0", "0/1", "1/1", "./."))))
dim(Geno.tab)

# table(QUAL= QUAL>1000, QD= QD>10)
# missing= Geno.tab["./.", ]/ncol(vcf1)<0.5
table(Quality= QUAL>1000 & QD>10 & Geno.tab["./.", ]/ncol(vcf1)<0.5,
      SNP= isSNV(vcf1, singleAltOnly= TRUE))
#####
filter1= 
  QUAL>1000 & QD>12 & # quality
  Geno.tab["./.", ]/ncol(vcf1)<0.5 & # missing
  isSNV(vcf1, singleAltOnly= TRUE) # SNP biallelic

vcf2= vcf1[filter1, ]
dim(vcf1); dim(vcf2)
##################################################
GT= geno(vcf2)$"GT"
GQ= geno(vcf2)$"GQ"
Geno.tab= apply(GT, 1, function(x) table(factor(x, levels= c("0/0", "0/1", "1/1", "./."))))
Sample.tab= apply(GT, 2, function(x) table(factor(x, levels= c("0/0", "0/1", "1/1", "./."))))

allele.freq= (Geno.tab["1/1", ]+ 0.5*Geno.tab["0/1", ])/(ncol(GT)-Geno.tab["./.", ])
allele.freq[allele.freq>0.5]= 1- allele.freq[allele.freq>0.5]
hist(allele.freq, breaks = seq(0,0.5,0.01))
table(allele.freq> 0.05)
##################################################
vcf3= vcf2[allele.freq> 0.05, ]
##################################################
sample.sheetx= subset(sample.sheet, country== "TW")
### Genotype to SNP score
GT= geno(vcf3)$"GT"
Geno.tab= apply(GT, 1, function(x) table(factor(x, levels= c("0/0", "0/1", "1/1", "./."))))

rownames(GT)= paste0(seqnames(rowRanges(vcf3)), ":", start(rowRanges(vcf3)), 
                     "_",
                     as.character(fixed(vcf3)$"REF"), "/", 
                     as.character(unlist(fixed(vcf3)$"ALT")))

SNP.num= matrix(NA, nrow= nrow(GT), ncol= ncol(GT),
                dimnames = list(rownames(GT), colnames(GT)))
dim(SNP.num)
SNP.num[1:5, 1:5]
SNP.num[which(GT=="0/0")]= -1
SNP.num[which(GT=="0/1")]= 0
SNP.num[which(GT=="1/1")]= 1

SNP.numt= t(SNP.num)
##################################################
#### SNP score > MRD dist matrix > PCoA
SNP.dist= dist(SNP.numt, method = "euclidean")
SNP.dist.adj= sqrt(SNP.dist^2/2)/ncol(SNP.numt)

mds.SNP= cmdscale(as.matrix(SNP.dist), k = nrow(SNP.numt)-1, eig= TRUE)
var.SNP= mds.SNP$"eig"/sum(mds.SNP$"eig")
SNP.distx= dist(mds.SNP$"points", method= "euclidean", diag = TRUE, upper = TRUE)

#### column centring
SNP.numc= scale(SNP.numt, center = TRUE, scale = FALSE)
SNP.numc[is.na(SNP.numc)]= 0
#### SVD
SNP.svd= svd(SNP.numc)
var.SVD= SNP.svd$"d"^2/sum(SNP.svd$"d"^2)
PCA.coord= SNP.numc %*% SNP.svd$"v"
# PCA.coordx= SNP.svd$"u" %*% diag(SNP.svd$"d")
SVD.dist= dist(PCA.coord, method= "euclidean", diag = TRUE, upper = TRUE)
##### prcomp
prcomp.pca= prcomp(SNP.numc, center = TRUE, scale. = FALSE)
var.prcomp= prcomp.pca$"sdev"^2/sum(prcomp.pca$"sdev"^2)
prcomp.dist= dist(prcomp.pca$"x", method= "euclidean", diag = TRUE, upper = TRUE)
##################################################
### PCoA
library(reshape2)
GT.m= melt(GT, value.name = "geno", varnames = c("SNP", "SID"))
head(GT.m)
GT.m= cbind(GT.m, colsplit(string = GT.m$"geno", pattern = "/", names = c("A1", "A2")))

GT.m2= melt(GT.m, id.vars = c("SNP", "SID"), measure.vars = c("A1", "A2"), 
            value.name = "allele", variable.name = "A.pos")

GT.m2$"allele"[GT.m2$"allele"=="."]= NA
GT.m2$"A.pos"= as.numeric(GT.m2$"A.pos")

GT.d= dcast(GT.m2, SID~ SNP+ allele, value.var = "allele", 
            fun.aggregate = function(x){
              if(sum(is.na(x))>0) NA else length(x)/2
            }
)

col.nmx= colsplit(string = colnames(GT.d), pattern = "_", names = c("snp", "a1", "a2"))
head(col.nmx)

GT.dx= GT.d
na.cols= which(is.na(col.nmx$"a2"))[-1]
length(na.cols)
# sum(is.na(GT.m2$"allele"))

for(x in na.cols){
  GT.dx[which(is.na(GT.dx[, x])), (x-2):(x-1)]=NA
  if(which(na.cols==x)%%2000==0) print(x)
}

rownames(GT.dx)= GT.dx[, 1]
GT.dxx= GT.dx[, -c(1, na.cols)]
MRD= dist(GT.dxx, method="euclidean", method= "euclidean", diag = TRUE, upper = TRUE)/sqrt(2*nrow(GT))
#####
mds.MRD= cmdscale(MRD, eig=TRUE, k = 5)
eig.mrd= mds.MRD$eig/sum(mds.MRD$eig[mds.MRD$eig>0])
var.MRD= round(eig.mrd[1:5], 4)
# [1] 0.6458 0.0461 0.0231 0.0183 0.0156

plot(eig.mrd, xlim= c(0, 15))

plot(x= mds.MRD$"points"[, 1], y= mds.MRD$"points"[, 2], col= sample.sheetx$"sub.population", asp= 1)
abline(h = 0, v = 0, col= "blue", lty= 3)
##################################################
##################################################
##################################################
dim(sample.sheetx)
head(sample.sheetx)
sprintf(fmt = "S%03d", sample.sheetx$"SID")
sample.labels= attr(MRD, which = "Labels")
dim(as.matrix(MRD))

as.matrix(MRD)[1:5, 1:5]
as.matrix(SNP.distx)[1:5, 1:5]
as.matrix(SVD.dist)[1:5, 1:5]
as.matrix(prcomp.dist)[1:5, 1:5]
##################################################

AllDist.df= data.frame(from= rep(sample.labels, times= length(sample.labels)),
                       to= rep(sample.labels, each= length(sample.labels)),
                       f.ix= rep(sample.sheetx$"SID", times= length(sample.labels)),
                       t.ix= rep(sample.sheetx$"SID", each= length(sample.labels)),
                       f.pop= rep(sample.sheetx$"sub.population", times= length(sample.labels)),
                       t.pop= rep(sample.sheetx$"sub.population", each= length(sample.labels)),
                       MRD= as.vector(as.matrix(MRD)),
                       SNP.dist= as.vector(as.matrix(SNP.distx)),
                       PCA.dist= as.vector(as.matrix(SVD.dist)),
                       prcomp.dist= as.vector(as.matrix(prcomp.dist)))

AllDist.dfx= subset(AllDist.df, f.ix< t.ix)
AllDist.dfx$"pop.type"= paste(AllDist.dfx$"f.pop", AllDist.dfx$"t.pop", sep = "-")
AllDist.dfx$"pop.typex"= factor(AllDist.dfx$"pop.type", levels= c("indica-indica", "indica-japonica", "japonica-indica", "japonica-japonica"))
levels(AllDist.dfx$"pop.typex")= c("indica-indica", "indica-japonica", "indica-japonica", "japonica-japonica")
head(AllDist.dfx)

ggplot(AllDist.dfx)+ 
  # geom_point(aes(x= PCA.dist, y= prcomp.dist, colour= pop.typex), size= 0.5)+
  # geom_abline(slope= 1, intercept = 0, colour= "pink")
  # geom_point(aes(x= PCA.dist, y= SNP.dist, colour= pop.typex), size= 0.5)+ # XXXXX it's wrong
  geom_point(aes(x= PCA.dist, y= MRD, colour= pop.typex), size= 0.5)+
  geom_abline(slope= 1/300, intercept = 0, colour= "pink")

lm(data = AllDist.dfx, PCA.dist~ MRD-1)
2*sqrt(nrow(GT))

##################################################
library(corehunter)
library(plyr)
library(doMC)
registerDoMC(2)

SNP.numx= SNP.num+1
SNP.numx[1:5, 1:5]
ccdist= distances(data = as.matrix(SVD.dist))
ccgeno= genotypes(data = t(SNP.numx), format= "biparental")
CHD= coreHunterData(genotypes = ccgeno, distances = ccdist)

size.cc= floor(90*seq(0.1, 0.4, 0.05)) #(size.cc= c(9, 18, 27, 36))
size.cc
sim= 100

(time0= Sys.time())
size.opt= ldply(size.cc, .progress = progress_text(), function(x){
  ldply(seq(sim), .id = "sim", function(sx){
    chobj.AN= sampleCore(ccgeno, objective("AN", measure = "MR", weight = 1), size= x, mode= "fast")
    eval.EN= evaluateCore(chobj.AN$"sel", ccgeno, objective("EN", measure = "MR", weight = 1))
    opt.AN= data.frame(eval.EN= eval.EN, eval.AN= chobj.AN$"AN"$"MR", size= x, AN.weight= 1)
    
    chobj.EN= sampleCore(ccgeno, objective("EN", measure = "MR", weight = 1), size= x, mode= "fast")
    eval.AN= evaluateCore(chobj.EN$"sel", ccgeno, objective("AN", measure = "MR", weight = 1))
    opt.EN= data.frame(eval.EN= chobj.EN$"EN"$"MR", eval.AN= eval.AN, size= x, AN.weight= 0)
    rbind(opt.EN, opt.AN)
  })
})
(time1= Sys.time())
time1-time0
ls()
setwd("/Users/energy/Dropbox/2017-12-07_practice/109_育種試驗資料分析/[Exercise] CoreCollection/")
save("CHD", "ccgeno", "size.cc", "size.opt", file = "size_optimum.Rdata")
# head(size.opt); dim(size.opt)
# rep(seq(10), each= length(size.cc)*2)

ggplot(size.opt)+ 
  geom_point(aes(x= eval.EN, y= eval.AN, colour= factor(size), shape= factor(AN.weight)))

size.opt.mean= ddply(size.opt, c("size", "AN.weight"), function(x) {
  data.frame(m.EN= mean(x$"eval.EN"), m.AN= mean(x$"eval.AN"))
})
head(size.opt.mean)
ggplot(size.opt.mean)+ 
  geom_point(aes(x= m.EN, y= m.AN, shape= factor(AN.weight), colour= factor(size)))

##################################################


weight= seq(0.25, 0.75, 0.25)
chobj.weight = llply(weight, .progress = progress_text(), function(x){
  sampleCore(ccgeno, 
             list(objective("AN", measure = "MR", weight = x), 
                  objective("EN", measure = "MR", weight = 1-x)), 
             size= 18, mode= "default")
})

AN <- sampleCore(ccgeno, list(objective("AN", measure = "MR", weight = 1, mode = "default")))
EN <- sampleCore(ccgeno, list(objective("EN", measure = "MR", weight = 1, mode = "default")))
evaEN <- evaluateCore(AN, ccgeno, objective("EN", measure = "MR"))
evaAN <- evaluateCore(EN, ccgeno, objective("AN", measure = "MR"))
sel <- data.frame("AN=0" = unlist(EN$sel),"AN=0.25" = unlist(chobj.weight[[1]][1]), 
                  "AN=0.5" = unlist(chobj.weight[[2]][1]),"AN=0.75" = unlist(chobj.weight[[3]][1]), "AN=1" = unlist(AN$sel))
MR <- data.frame("AN" = c(evaAN, chobj.weight[[1]][2],chobj.weight[[2]][2],chobj.weight[[3]][2], EN$EN),
                 "EN" = c(EN$EN, chobj.weight[[1]][3],chobj.weight[[2]][3], chobj.weight[[3]][3], evaEN))
MRplot <- matrix(MR,2,5, byrow = T)
rownames(MRplot) <- c("AN", "EN")
colnames(MRplot) <- c("ANE = 0", "ANE = 0.25", "ANE = 0.5", "ANE = 0.75", "ANE = 1")
plot(MRplot[1,],MRplot[2,], xlab = "AN", ylab = "EN", cex = 1.5, col = "blue", pch = 16, 
     xlim = c(0.31,0.39), ylim = c(0.31,0.39), main = "AN vs EN")
lines(MRplot[1,],MRplot[2,], col = "blue", lty = 3)
legend("bottomright", legend = "weight of ANE", box.lty = 3, cex = 1, col = "blue", pch = 16)
text(MRplot[1,], MRplot[2,],labels=colnames(MRplot), cex= 0.75, pos = 2)

plot(PCA.coord, xlab= "PC1", ylab = "PC2", main = "PC analysis (ANE = 0 or = 1)", col = "red", pch = 16)
points(PCA.coord[as.character(sel[,1]),], col = "blue", pch = 16)
legend("bottomright", legend = c("core collection", "population"), col = c("blue", "red"), pch = 16)
plot(PCA.coord, xlab= "PC1", ylab = "PC2", main = "PC analysis (ANE = 0.5)", col = "red", pch = 16)
points(PCA.coord[as.character(sel[,3]),], col = "blue", pch = 16)
legend("bottomright", legend = c("core collection", "population"), col = c("blue", "red"), pch = 16)


weight.df= ldply(chobj.weight, function(x) data.frame(ENE= x$"EN"$"MR", ANE= x$"AN"$"MR"))
plot(weight.df$"ENE", weight.df$"ANE")


library(ggplot2)
ggplot()+ 
  geom_point(aes(x= size, y= eval.AN, colour= type))

ggplot(problem1)+ 
  geom_point(aes(x= size, y= eval.EN, colour= type))

ggplot(problem1)+ geom_point(aes(x= eval.EN, y= eval.AN, colour= factor(size)))