## Data examination 
############################################################
## We have done this at the very begining of the mapping process
## You have done a in-depth check for genotypic data before the construction of the linkage map
## Now, let us go back to the phenotypic data before QTL mapping

nphe(cro) # No. of phenotype (including "ind")
phenames(cro) # trait name


## A graphical summary of the cross object
plot(cro)

## You may want to see a larger figure
## One option:

pdf("summary_cro.pdf")
plotMissing(cro)
plotMap(cro)
for (i in 1:16) {plotPheno(cro, pheno.col=i)}
dev.off()

## Observe the phenotypes, what would you say about the distribution and the value?
## Why do they look such? Can you find some information in the original paper or on the data website?
## Are you going to use the data as they are or do some cleaning?

## If you have made decisions on the genotypic data and phenotypic data to use, 
## then we can proceed to the QTL mapping.


############################################################
# Single-QTL analysis
############################################################
cro <- calc.genoprob(cro, step=1, map.function = "kosambi")

out.em <- scanone(cro, pheno.col= 1:16, method="em") # use EM algorithme (the original one)

summary(out.em) # use ?summary.scanone to see the meaning of the output
str(out.em)	# Check what you have produced
head(out.em)

out.hk <- scanone(cro,  pheno.col= 1:16, method="hk")  # A faster method

#---------------------------------------------------------#
# Compare the results between EM and HK
#---------------------------------------------------------#
pdf("EM_HK.pdf")
for( i in 1:16)
{
  plot(out.em, col="blue", lodcolumn=i, main = names(out.em)[i+2])
  plot(out.hk, col="red" , lodcolumn=i, lty = 2, add=TRUE)
  legend("topleft", legend = c("EM", "HK"), bty = "n", lty = c(1,2), lwd = 2, col = c("blue", "red"))
}

dev.off()

## What do you think about these two methods?


## Test of another method
cro <- sim.geno(cro, step=1, n.draws=64)
out.imp <- scanone(cro, method="imp", pheno.col = 1:16) 

#------------------------------------------------------------------#
# Compare the results between EM and imputation (+HK if you want)
#------------------------------------------------------------------#
pdf("EM_IMP.pdf")
for( i in 1:16)
{
  plot(out.em, col="blue", lodcolumn=i, main = names(out.em)[i+2])
  plot(out.imp, col="orange" , lodcolumn=i, lty = 2, add=TRUE)
  legend("topleft", legend = c("EM", "IMP"), bty = "n", lty = c(1,2), lwd = 2, col = c("blue", "orange"))
}

dev.off()


#------------------------------------------------------------------#
# Permutation tests
#------------------------------------------------------------------#
# Let us take the faster calculation
system.time(operm.hk <- scanone(cro, method="hk", n.perm=1000, pheno.col = 1:16)) 
#35.21 sec for Oakeley for all 16 traits and the full data set

## Have a look of the object
plot(operm.hk, lodcolumn = 1 )

summary(operm.hk, alpha=c(0.05, 0.2))

summary(out.hk, format = "allpheno", perms = operm.hk, alpha = 0.05)
summary(out.hk, format = "tabByCol", perms = operm.hk, alpha = 0.05)

pdf("HK_SIM.pdf")
for( i in 1:16)
{
  plot(out.hk, lodcolumn=i, main = names(out.em)[i+2])
  add.threshold(out.hk, lodcolumn=i, perms = operm.hk, alpha = 0.05)
}
dev.off()

# Permutation for EM
system.time(operm.em <- scanone(cro, method="em", n.perm=1000, pheno.col = 1:16)) 
# 2505.37 sec in HYF's PC ~ 40 min


############################################################
# Interval estimates of QTL location
############################################################
lodint(out.hk, chr = 3, drop=1.5)
bayesint(out.hk, chr=3, prob=0.95)

lodint(out.hk, chr=3, expandtomarkers=TRUE)
bayesint(out.hk, chr=3, prob=0.95, expandtomarkers=TRUE)

lodint(out.hk, chr=3, drop=2, lodcolumn=1)
bayesint(out.hk, chr=3, prob=0.99)





############################################################
# QTL effects
############################################################
## A way to look into the marker effect 
max(out.hk, lodcolumn=1)
mar <-  find.marker(cro, chr=3, pos=26)
plotPXG(cro, marker=mar)

effectplot(cro, mname1=mar)
effectplot(cro, mname1="3@26")



############################################################
# Two-dimensional, two-QTL scans
############################################################
cro2 <- calc.genoprob(cro, step=2)
out2 <- scantwo(cro2, method="hk")

plot(out2)
plot(out2, lower="fv1")
plot(out2, lower="fv1", upper="av1")

system.time(operm2 <- scantwo(cro2, method="hk", n.perm=1000)) # in batch mode, so it is faster: 247.34 sec

summary(out2, perms=operm2, alpha=0.2, pvalues=TRUE)

############################################################
# Multiple-QTL analyses
############################################################
# I recommend you to follow the tutorial on the R/qtl webpage "New functions for exploring multiple-QTL models"
qtl <- makeqtl(cro, chr=c(3,5), pos=c(26, 67), what="prob")

out.fq <- fitqtl(cro, qtl=qtl, method="hk")
summary(out.fq)

summary(fitqtl(cro, qtl=qtl, method="hk", get.ests=TRUE, dropone=FALSE))

out.fqi1 <- fitqtl(cro, qtl=qtl, method="hk", formula=y~Q1*Q2)
out.fqi2 <- fitqtl(cro, qtl=qtl, method="hk", formula=y~Q1+Q2+Q1:Q2)
summary(out.fqi2)

addint(cro, qtl=qtl, method="hk")

rqtl <- refineqtl(cro, qtl=qtl, method="hk")
rqtl

summary(out.fqr <- fitqtl(cro, qtl=rqtl, method="hk"))

plotLodProfile(rqtl)

plot(out.hk, chr=c(3,5), col="red", add=TRUE)

out.aq <- addqtl(cro, qtl=rqtl, method="hk")

plot(out.aq)

print(pen <- calc.penalties(operm2))

out.sq3 <- stepwiseqtl(cro, pheno.col = 6, max.qtl=10, penalties=pen, method="hk", verbose=2)
out.sq

save.image("r_oakley2.RData")
