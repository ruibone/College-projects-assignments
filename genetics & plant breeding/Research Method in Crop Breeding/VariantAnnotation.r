library(VariantAnnotation)
library(ABHgenotypeR)
library(ggplot2)

##read vcf+write origin csv
ref.vcf <- readVcf("populations_resultsRef.snps.vcf")#53402    48
ref.snp <- ref.vcf[isSNV(ref.vcf),]#53402    48
ref.miss <- (48-ref.vcf@info$NS)/48
mean(ref.miss)#0.4689
ref.DP <- geno(ref.vcf)$DP
ref.GT <- geno(ref.vcf)$GT
which(which(is.na(ref.DP)==T)%in%which(ref.GT=="./.")==F)#integer(0)
which(which(is.na(ref.DP)==F)%in%which(ref.GT=="./.")==T)#integer(0)
which(which(is.na(ref.DP)==T)%in%which(!ref.GT=="./.")==T)#integer(0)
ref.ABH <- ref.GT
ref.ABH <- gsub("0/0","A",ref.ABH)
ref.ABH <- gsub("0/1","H",ref.ABH)
ref.ABH <- gsub("1/1","B",ref.ABH)
ref.ABH <- gsub("./.",".",ref.ABH)
ref.ABH <- t(ref.ABH)
ref.ABH.fin <- rbind(factor(ref.vcf@rowRanges@seqnames),ref.ABH)
colnames(ref.ABH.fin) <- paste0("S",ref.ABH.fin[1,],"_",ref.vcf@rowRanges@ranges@start)
write.csv(ref.ABH.fin,"ref_ABH_fin.csv",quote = F,na=".",row.names=T)
ref.ABH.df <- readABHgenotypes("ref_ABH_fin.csv",nameA = "A",nameB = "B")
ref.ABH.df.imp <- imputeByFlanks(ref.ABH.df)
plotGenos(ref.ABH.df)

###identify truely chromosome
X <- which(table(ref.ABH.fin[1,])>1000)
ref.ABH.fil <- ref.ABH.fin[,which(ref.ABH.fin[1,]%in%as.numeric(names(X))==T)]
write.csv(ref.ABH.fil,"ref_ABH_fil.csv",quote = F,na=".",row.names=T)

###write csv
filll.vcf<- fil.vcf[fil.miss<0.1
                    & unlist(fil.AF>0.2)
                    & unlist(fil.AF<0.8),]
filll.DP <- geno(filll.vcf)$DP
filll.ABH <- geno(filll.vcf)$GT
filll.ABH <- gsub("0/0","A",filll.ABH)
filll.ABH <- gsub("0/1","H",filll.ABH)
filll.ABH <- gsub("1/1","B",filll.ABH)
filll.ABH <- gsub("./.",".",filll.ABH)
filll.ABH <- t(filll.ABH)
filll.ABH.fin <- rbind(factor(filll.vcf@rowRanges@seqnames),filll.ABH)
colnames(filll.ABH.fin) <- paste0("S",filll.ABH.fin[1,],"_",filll.vcf@rowRanges@ranges@start)
write.csv(filll.ABH.fin,"filll_ABH_fin.csv",quote = F,na=".",row.names=T)